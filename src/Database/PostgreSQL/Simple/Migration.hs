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
    , runMigrationA
    , runMigrations
    , runMigrationsA
    , sequenceMigrations

    -- * Logging
    , defaultLogWrite

    -- * Migration types
    , MigrationContext(..)
    , MigrationCommand(..)
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
import           Control.Monad                      (void, when)
import qualified Crypto.Hash.MD5                    as MD5 (hash)
import qualified Data.ByteString                    as BS (ByteString, readFile)
import qualified Data.ByteString.Base64             as B64 (encode)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T (putStrLn, hPutStrLn)
import           Data.String                        (fromString)
import           Data.Foldable                      (Foldable)
import           Data.List                          (isPrefixOf, sort)
import           Data.Traversable                   (Traversable)
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

defaultLogWrite :: Either T.Text T.Text -> IO ()
defaultLogWrite = either (T.hPutStrLn stderr) T.putStrLn

-- | Executes migrations inside the provided 'MigrationContext'.
--
-- Returns 'MigrationSuccess' if the provided 'MigrationCommand' executes
-- without error. If an error occurs, execution is stopped and
-- a 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigration' inside a database transaction.
runMigration :: MigrationContext -> IO (MigrationResult String)
runMigration = runMigrationA defaultLogWrite

-- | A version of 'runMigration' which gives you control of where the log
-- messages are sent to.
runMigrationA
    :: (Either T.Text T.Text -> IO ())
       -- ^ Log write function. 'Either' indicates log level,
       -- 'Left' for an error message and 'Right' for an info message.
    -> MigrationContext
    -> IO (MigrationResult String)
runMigrationA logWrite (MigrationContext cmd verbose con) = case cmd of
    MigrationInitialization ->
        initializeSchema logWrite con verbose >> return MigrationSuccess
    MigrationDirectory path ->
        executeDirectoryMigration logWrite con verbose path
    MigrationScript name contents ->
        executeMigration logWrite con verbose name contents
    MigrationFile name path ->
        executeMigration logWrite con verbose name =<< BS.readFile path
    MigrationValidation validationCmd ->
        executeValidation logWrite con verbose validationCmd
    MigrationCommands commands ->
        runMigrationsA logWrite verbose con commands

-- | Execute a sequence of migrations
--
-- Returns 'MigrationSuccess' if all of the provided 'MigrationCommand's
-- execute without error. If an error occurs, execution is stopped and the
-- 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigrations' inside a database transaction.
runMigrations
    :: Bool
       -- ^ Run in verbose mode
    -> Connection
       -- ^ The postgres connection to use
    -> [MigrationCommand]
       -- ^ The commands to run
    -> IO (MigrationResult String)
runMigrations = runMigrationsA defaultLogWrite

-- | A version of 'runMigrations' which gives you control of where the log
-- messages are sent to.
runMigrationsA
    :: (Either T.Text T.Text -> IO ())
       -- ^ Log write function. 'Either' indicates log level,
       -- 'Left' for an error message and 'Right' for an info message.
    -> Bool
       -- ^ Run in verbose mode
    -> Connection
       -- ^ The postgres connection to use
    -> [MigrationCommand]
       -- ^ The commands to run
    -> IO (MigrationResult String)
runMigrationsA logWrite verbose con commands =
    sequenceMigrations
        [ runMigrationA logWrite (MigrationContext c verbose con)
        | c <- commands
        ]

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
    :: LogWrite
    -> Connection
    -> Bool
    -> FilePath
    -> IO (MigrationResult String)
executeDirectoryMigration logWrite con verbose dir =
    scriptsInDirectory dir >>= go
    where
        go fs = sequenceMigrations (executeMigrationFile <$> fs)
        executeMigrationFile f =
            BS.readFile (dir ++ "/" ++ f) >>=
                executeMigration logWrite con verbose f

-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [String]
scriptsInDirectory dir =
    fmap (sort . filter (\x -> not $ "." `isPrefixOf` x))
        (getDirectoryContents dir)

-- | Executes a generic SQL migration for the provided script 'name' with
-- content 'contents'.
executeMigration
    :: LogWrite
    -> Connection
    -> Bool
    -> ScriptName
    -> BS.ByteString
    -> IO (MigrationResult String)
executeMigration logWrite con verbose name contents = do
    let checksum = md5Hash contents
    checkScript con name checksum >>= \case
        ScriptOk -> do
            when verbose $ logWrite $ Right $ "Ok:\t" <> fromString name
            return MigrationSuccess
        ScriptNotExecuted -> do
            void $ execute_ con (Query contents)
            void $ execute con q (name, checksum)
            when verbose $ logWrite $ Right $ "Execute:\t" <> fromString name
            return MigrationSuccess
        ScriptModified { actual, expected } -> do
            when verbose $ logWrite $ Left
                $ "Fail:\t" <> fromString name
                <> "\n" <> scriptModifiedErrorMessage expected actual
            return (MigrationError name)
    where
        q = "insert into schema_migrations(filename, checksum) values(?, ?)"

-- | Initializes the database schema with a helper table containing
-- meta-information about executed migrations.
initializeSchema :: LogWrite -> Connection -> Bool -> IO ()
initializeSchema logWrite con verbose = do
    when verbose $ logWrite $ Right "Initializing schema"
    void $ execute_ con $ mconcat
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
    :: LogWrite
    -> Connection
    -> Bool
    -> MigrationCommand
    -> IO (MigrationResult String)
executeValidation logWrite con verbose cmd = case cmd of
    MigrationInitialization ->
        existsTable con "schema_migrations" >>= \r -> return $ if r
            then MigrationSuccess
            else MigrationError "No such table: schema_migrations"
    MigrationDirectory path ->
        scriptsInDirectory path >>= goScripts path
    MigrationScript name contents ->
        validate name contents
    MigrationFile name path ->
        validate name =<< BS.readFile path
    MigrationValidation _ ->
        return MigrationSuccess
    MigrationCommands cs ->
        sequenceMigrations (executeValidation logWrite con verbose <$> cs)
    where
        validate name contents =
            checkScript con name (md5Hash contents) >>= \case
                ScriptOk -> do
                    when verbose $ logWrite $ Right $ "Ok:\t" <> fromString name
                    return MigrationSuccess
                ScriptNotExecuted -> do
                    when verbose $ logWrite $ Left $
                        "Missing:\t" <> fromString name
                    return (MigrationError $ "Missing: " ++ name)
                ScriptModified { expected, actual } -> do
                    when verbose $ logWrite $ Left
                        $ "Checksum mismatch:\t" <> fromString name
                        <> "\n" <> scriptModifiedErrorMessage expected actual
                    return (MigrationError $ "Checksum mismatch: " ++ name)

        goScripts path xs = sequenceMigrations (goScript path <$> xs)
        goScript path x = validate x =<< BS.readFile (path ++ "/" ++ x)

-- | Checks the status of the script with the given name 'name'.
-- If the script has already been executed, the checksum of the script
-- is compared against the one that was executed.
-- If there is no matching script entry in the database, the script
-- will be executed and its meta-information will be recorded.
checkScript :: Connection -> ScriptName -> Checksum -> IO CheckScriptResult
checkScript con name fileChecksum =
    query con q (Only name) >>= \case
        [] ->
            return ScriptNotExecuted
        Only dbChecksum:_ | fileChecksum == dbChecksum ->
            return ScriptOk
        Only dbChecksum:_ ->
            return (ScriptModified {
                       expected = dbChecksum,
                       actual = fileChecksum
                    })
    where
        q = mconcat
            [ "select checksum from schema_migrations "
            , "where filename = ? limit 1"
            ]

-- | Calculates the MD5 checksum of the provided bytestring in base64
-- encoding.
md5Hash :: BS.ByteString -> Checksum
md5Hash = B64.encode . MD5.hash

-- | Log write function.
--
-- 'Either' indicates log level,
-- 'Left' for an error message and 'Right' for an info message.
type LogWrite = Either T.Text T.Text -> IO ()

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
scriptModifiedErrorMessage expected actual
    = "expected: " <> fromString (show expected)
    <> "\nhash was: " <> fromString (show actual)

-- | A sum-type denoting the result of a migration.
data MigrationResult a
    = MigrationError a
    -- ^ There was an error in script migration.
    | MigrationSuccess
    -- ^ All scripts have been executed successfully.
    deriving (Show, Eq, Read, Ord, Functor, Foldable, Traversable)

-- | The 'MigrationContext' provides an execution context for migrations.
data MigrationContext = MigrationContext
    { migrationContextCommand    :: MigrationCommand
    -- ^ The action that will be performed by 'runMigration'
    , migrationContextVerbose    :: Bool
    -- ^ Verbosity of the library.
    , migrationContextConnection :: Connection
    -- ^ The PostgreSQL connection to use for migrations.
    }

-- | Produces a list of all executed 'SchemaMigration's.
getMigrations :: Connection -> IO [SchemaMigration]
getMigrations = flip query_ q
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
