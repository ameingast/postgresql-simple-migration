-- |
-- Module      : Database.PostgreSQL.Simple.Migration
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A migration library for postgresql-simple.
--
-- For usage, see Readme.markdown.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Database.PostgreSQL.Simple.Migration
    (
    -- * Migration actions
    runMigration
    , runMigrations
    , sequenceMigrations

    -- * Migration types
    , MigrationContext(..)
    , MigrationCommand(..)
    , MigrationVerbosity(..)
    , MigrationResult(..)
    , ScriptName
    , Checksum

    -- * Migration result actions
    , getMigrations

    -- * Migration result types
    , SchemaMigration(..)
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                ((<$>), (<*>))
#endif
import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import qualified Crypto.Hash.MD5                    as MD5 (hash)
import qualified Data.ByteString                    as BS (ByteString, readFile)
import qualified Data.ByteString.Base64             as B64 (encode)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Data.String                        (fromString)
import           Data.List                          (isPrefixOf, sort)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid                        (Monoid (..))
#endif
import           Data.Time                          (LocalTime)
import           Database.PostgreSQL.Simple         (Connection, Only (..),
                                                     execute, execute_, query,
                                                     query_)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PostgreSQL.Simple.Types   (Query (..))
import           Database.PostgreSQL.Simple.Util    (existsTable)
import           System.Directory                   (getDirectoryContents)
import           System.IO                          (stderr)

-- | Executes migrations inside the provided 'MigrationContext'.
--
-- Returns 'MigrationSuccess' if the provided 'MigrationCommand' executes
-- without error. If an error occurs, execution is stopped and
-- a 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigration' inside a database transaction.
runMigration
    :: (MonadIO m, MigrationVerbosity verbosity)
    => MigrationContext verbosity
    -> m (MigrationResult String)
runMigration (MigrationContext cmd verbosity con) = case cmd of
    MigrationInitialization ->
        initializeSchema con verbosity >> return MigrationSuccess
    MigrationDirectory path ->
        executeDirectoryMigration con verbosity path
    MigrationScript name contents ->
        executeMigration con verbosity name contents
    MigrationFile name path ->
        executeMigration con verbosity name =<< liftIO (BS.readFile path)
    MigrationValidation validationCmd ->
        executeValidation con verbosity validationCmd
    MigrationCommands commands ->
        runMigrations verbosity con commands

-- | Execute a sequence of migrations
--
-- Returns 'MigrationSuccess' if all of the provided 'MigrationCommand's
-- execute without error. If an error occurs, execution is stopped and the
-- 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigrations' inside a database transaction.
runMigrations
    :: (MonadIO m, MigrationVerbosity verbosity)
    => verbosity
       -- ^ Verbosity control (e.g. 'Bool')
    -> Connection
       -- ^ The postgres connection to use
    -> [MigrationCommand]
       -- ^ The commands to run
    -> m (MigrationResult String)
runMigrations verbosity con commands =
    sequenceMigrations
        [runMigration (MigrationContext c verbosity con) | c <- commands]

-- | Run a sequence of contexts, stopping on the first failure
sequenceMigrations
    :: Monad m
    => [m (MigrationResult e)]
    -> m (MigrationResult e)
sequenceMigrations = \case
    []   -> return MigrationSuccess
    c:cs -> do
        r <- c
        case r of
          MigrationError s -> return (MigrationError s)
          MigrationSuccess -> sequenceMigrations cs

-- | Executes all SQL-file based migrations located in the provided 'dir'
-- in alphabetical order.
executeDirectoryMigration
    :: (MonadIO m, MigrationVerbosity verbosity)
    => Connection
    -> verbosity
    -> FilePath
    -> m (MigrationResult String)
executeDirectoryMigration con verbosity dir =
    scriptsInDirectory dir >>= go
    where
        go fs = sequenceMigrations (executeMigrationFile <$> fs)
        executeMigrationFile f =
            executeMigration con verbosity f
                =<< liftIO (BS.readFile $ dir ++ "/" ++ f)

-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: MonadIO m => FilePath -> m [String]
scriptsInDirectory dir =
    sort . filter (\x -> not $ "." `isPrefixOf` x)
        <$> liftIO (getDirectoryContents dir)

-- | Executes a generic SQL migration for the provided script 'name' with
-- content 'contents'.
executeMigration
    :: (MonadIO m, MigrationVerbosity verbosity)
    => Connection
    -> verbosity
    -> ScriptName
    -> BS.ByteString
    -> m (MigrationResult String)
executeMigration con verbosity name contents = do
    let checksum = md5Hash contents
    checkScript con name checksum >>= \case
        ScriptOk -> do
            migrationLogWrite verbosity $ Right ("Ok:\t" <> fromString name)
            return MigrationSuccess
        ScriptNotExecuted -> do
            void $ liftIO $ execute_ con (Query contents)
            void $ liftIO $ execute con q (name, checksum)
            migrationLogWrite verbosity $ Left ("Execute:\t" <> fromString name)
            return MigrationSuccess
        ScriptModified { actual, expected } -> do
            migrationLogWrite verbosity $ Left
              $ "Fail:\t" <> fromString name
              <> "\n" <> scriptModifiedErrorMessage expected actual
            return (MigrationError name)
    where
        q = "insert into schema_migrations(filename, checksum) values(?, ?)"

-- | Initializes the database schema with a helper table containing
-- meta-information about executed migrations.
initializeSchema
    :: (MonadIO m, MigrationVerbosity verbosity)
    => Connection
    -> verbosity
    -> m ()
initializeSchema con verbosity = do
    migrationLogWrite verbosity $ Right "Initializing schema"
    void $ liftIO $ execute_ con $ mconcat
        [ "create table if not exists schema_migrations "
        , "( filename varchar(512) not null"
        , ", checksum varchar(32) not null"
        , ", executed_at timestamp without time zone not null default now() "
        , ");"
        ]

-- | Validates a 'MigrationCommand'. Validation is defined as follows for these
-- types:
--
-- * 'MigrationInitialization': validate the presence of the meta-information
-- table.
-- * 'MigrationDirectory': validate the presence and checksum of all scripts
-- found in the given directory.
-- * 'MigrationScript': validate the presence and checksum of the given script.
-- * 'MigrationFile': validate the presence and checksum of the given file.
-- * 'MigrationValidation': always succeeds.
-- * 'MigrationCommands': validates all the sub-commands stopping at the first
-- failure.
executeValidation
    :: (MonadIO m, MigrationVerbosity verbosity)
    => Connection
    -> verbosity
    -> MigrationCommand
    -> m (MigrationResult String)
executeValidation con verbosity cmd = case cmd of
    MigrationInitialization ->
        existsTable con "schema_migrations" >>= \r -> return $ if r
            then MigrationSuccess
            else MigrationError "No such table: schema_migrations"
    MigrationDirectory path ->
        scriptsInDirectory path >>= goScripts path
    MigrationScript name contents ->
        validate name contents
    MigrationFile name path ->
        validate name =<< liftIO (BS.readFile path)
    MigrationValidation _ ->
        return MigrationSuccess
    MigrationCommands cs ->
        sequenceMigrations (executeValidation con verbosity <$> cs)
    where
        validate name contents =
            checkScript con name (md5Hash contents) >>= \case
                ScriptOk -> do
                    migrationLogWrite verbosity $
                        Right ("Ok:\t" <> fromString name)
                    return MigrationSuccess
                ScriptNotExecuted -> do
                    migrationLogWrite verbosity $
                        Left ("Missing:\t" <> fromString name)
                    return (MigrationError $ "Missing: " ++ name)
                ScriptModified { expected, actual } -> do
                    migrationLogWrite verbosity $ Left
                      $ "Checksum mismatch:\t" <> fromString name
                      <> "\n" <> scriptModifiedErrorMessage expected actual
                    return (MigrationError $ "Checksum mismatch: " ++ name)

        goScripts path xs = sequenceMigrations (goScript path <$> xs)
        goScript path x = validate x =<< liftIO (BS.readFile $ path ++ "/" ++ x)

-- | Checks the status of the script with the given name 'name'.
-- If the script has already been executed, the checksum of the script
-- is compared against the one that was executed.
-- If there is no matching script entry in the database, the script
-- will be executed and its meta-information will be recorded.
checkScript
    :: MonadIO m
    => Connection
    -> ScriptName
    -> Checksum
    -> m CheckScriptResult
checkScript con name fileChecksum =
    f <$> liftIO (query con q $ Only name)
    where
        q = mconcat
            [ "select checksum from schema_migrations "
            , "where filename = ? limit 1"
            ]

        f [] = ScriptNotExecuted
        f (Only dbChecksum:_)
            | fileChecksum == dbChecksum = ScriptOk
            | otherwise =
                ScriptModified
                    { expected = dbChecksum
                    , actual   = fileChecksum
                    }

-- | Calculates the MD5 checksum of the provided bytestring in base64
-- encoding.
md5Hash :: BS.ByteString -> Checksum
md5Hash = B64.encode . MD5.hash

-- | The checksum type of a migration script.
type Checksum = BS.ByteString

-- | The name of a script. Typically the filename or a custom name
-- when using Haskell migrations.
type ScriptName = String

-- | 'MigrationCommand' determines the action of the 'runMigration' script.
data MigrationCommand
    = MigrationInitialization
    -- ^ Initializes the database with a helper table containing meta
    -- information.
    | MigrationDirectory FilePath
    -- ^ Executes migrations based on SQL scripts in the provided 'FilePath'
    -- in alphabetical order.
    | MigrationFile ScriptName FilePath
    -- ^ Executes a migration based on script located at the provided
    -- 'FilePath'.
    | MigrationScript ScriptName BS.ByteString
    -- ^ Executes a migration based on the provided bytestring.
    | MigrationValidation MigrationCommand
    -- ^ Validates that the provided MigrationCommand has been executed.
    | MigrationCommands [MigrationCommand]
    -- ^ Performs a series of 'MigrationCommand's in sequence.
    deriving (Show, Eq, Read, Ord)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup MigrationCommand where
    (<>) = mappend
#endif

instance Monoid MigrationCommand where
    mempty = MigrationCommands []
    mappend (MigrationCommands xs) (MigrationCommands ys) =
        MigrationCommands (xs ++ ys)
    mappend (MigrationCommands xs) y = MigrationCommands (xs ++ [y])
    mappend x (MigrationCommands ys) = MigrationCommands (x : ys)
    mappend x y = MigrationCommands [x, y]

-- | A sum-type denoting the result of a single migration.
data CheckScriptResult
    = ScriptOk
    -- ^ The script has already been executed and the checksums match.
    -- This is good.
    | ScriptModified { expected :: Checksum, actual :: Checksum }
    -- ^ The script has already been executed and there is a checksum
    -- mismatch. This is bad.
    | ScriptNotExecuted
    -- ^ The script has not been executed, yet. This is good.
    deriving (Show, Eq, Read, Ord)

scriptModifiedErrorMessage :: Checksum -> Checksum -> T.Text
scriptModifiedErrorMessage expected actual =
  "expected: " <> fromString (show expected) <>
  "\nhash was: " <> fromString (show actual)

-- | A sum-type denoting the result of a migration.
data MigrationResult a
    = MigrationError a
    -- ^ There was an error in script migration.
    | MigrationSuccess
    -- ^ All scripts have been executed successfully.
    deriving (Show, Eq, Read, Ord, Functor, Foldable, Traversable)

-- | The 'MigrationContext' provides an execution context for migrations.
data MigrationContext verbose
    = MigrationContext
    { migrationContextCommand    :: MigrationCommand
    -- ^ The action that will be performed by 'runMigration'.
    , migrationContextVerbose    :: verbose
    -- ^ Verbosity of the library (e.g. 'Bool').
    , migrationContextConnection :: Connection
    -- ^ The PostgreSQL connection to use for migrations.
    }

-- | Produces a list of all executed 'SchemaMigration's.
getMigrations :: MonadIO m => Connection -> m [SchemaMigration]
getMigrations = liftIO . flip query_ q
    where q = mconcat
            [ "select filename, checksum, executed_at "
            , "from schema_migrations order by executed_at asc"
            ]

-- | A product type representing a single, executed 'SchemaMigration'.
data SchemaMigration = SchemaMigration
    { schemaMigrationName       :: BS.ByteString
    -- ^ The name of the executed migration.
    , schemaMigrationChecksum   :: Checksum
    -- ^ The calculated MD5 checksum of the executed script.
    , schemaMigrationExecutedAt :: LocalTime
    -- ^ A timestamp without timezone of the date of execution of the script.
    } deriving (Show, Eq, Read)

instance Ord SchemaMigration where
    compare (SchemaMigration nameLeft _ _) (SchemaMigration nameRight _ _) =
        compare nameLeft nameRight

instance FromRow SchemaMigration where
    fromRow = SchemaMigration <$>
        field <*> field <*> field

instance ToRow SchemaMigration where
    toRow (SchemaMigration name checksum executedAt) =
       [toField name, toField checksum, toField executedAt]

-- | An abstract interface for handling logging.
--
-- If you need to use a logging framework consider this example:
--
-- @
-- data MyLogger = MyLogger Handle
--
-- instance MigrationVerbosity MyLogger where
--   migrationLogWrite (MyLogger h) = liftIO . T.hPutStrLn h . either id id
--
-- applyMigration :: Connection -> IO ()
-- applyMigration conn =
--   void . runMigration $
--     MigrationContext MigrationInitialization (MyLogger stderr) conn
-- @
class MigrationVerbosity verbosity where
    migrationLogWrite
        :: MonadIO m
        => verbosity
        -> Either T.Text T.Text
        -- ^ Either 'Left' for error log (e.g. stderr)
        -- or 'Right' for info log (e.g. stdout)
        -> m ()

-- | Default log write implementaion.
--
-- Either 'False' for quite mode or 'True' for verbose mode.
instance MigrationVerbosity Bool where
    migrationLogWrite False _          = pure ()
    migrationLogWrite True (Left  msg) = liftIO $ T.hPutStrLn stderr msg
    migrationLogWrite True (Right msg) = liftIO $ T.putStrLn msg
