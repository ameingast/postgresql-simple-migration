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
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Database.PostgreSQL.Simple.Migration
    (
    -- * Migration actions
      runMigration'
    , runMigration
    , runMigrations
    , runMigrations'
    , sequenceMigrations

    -- * Migration types
    , MigrationContext(..)
    , MigrationContext'(..)
    , MigrationCommand(..)
    , MigrationResult(..)
    , ScriptName
    , Checksum

    -- * Migration result actions
    , getMigrations
    , getMigrations'

    -- * Migration result types
    , SchemaMigration(..)
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                ((<$>), (<*>))
#endif
import           Control.Monad                      (void, when)
import qualified Crypto.Hash.MD5                    as MD5 (hash)
import qualified Data.ByteString                    as BS (ByteString, readFile)
import qualified Data.ByteString.Char8              as BS8 (unpack)
import qualified Data.ByteString.Base64             as B64 (encode)
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

-- | Executes migrations inside the provided 'MigrationContext'.
--
-- Returns 'MigrationSuccess' if the provided 'MigrationCommand' executes
-- without error. If an error occurs, execution is stopped and
-- a 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigration' inside a database transaction.
runMigration :: MigrationContext -> IO (MigrationResult String)
runMigration (MigrationContext cmd verbose con) = 
  runMigration' (MigrationContext' cmd verbose con "schema_migrations")

runMigration' :: MigrationContext' -> IO (MigrationResult String)
runMigration' (MigrationContext' cmd verbose con tableName) = case cmd of
    MigrationInitialization ->
        initializeSchema con tableName verbose >> return MigrationSuccess
    MigrationDirectory path ->
        executeDirectoryMigration con tableName verbose path
    MigrationScript name contents ->
        executeMigration con tableName verbose name contents
    MigrationFile name path ->
        executeMigration con tableName verbose name =<< BS.readFile path
    MigrationValidation validationCmd ->
        executeValidation con tableName verbose validationCmd
    MigrationCommands commands ->
        runMigrations' verbose con commands tableName

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
runMigrations verbose con commands = runMigrations' verbose con commands "schema_migrations"

-- | Execute a sequence of migrations
--
-- Returns 'MigrationSuccess' if all of the provided 'MigrationCommand's
-- execute without error. If an error occurs, execution is stopped and the
-- 'MigrationError' is returned.
--
-- It is recommended to wrap 'runMigrations' inside a database transaction.
runMigrations'
    :: Bool
       -- ^ Run in verbose mode
    -> Connection
       -- ^ The postgres connection to use
    -> [MigrationCommand]
       -- ^ The commands to run
    -> BS.ByteString
       -- ^ The schema_migrations table name
    -> IO (MigrationResult String)
runMigrations' verbose con commands tableName =
    sequenceMigrations [runMigration' (MigrationContext' c verbose con tableName) | c <- commands]

-- | Run a sequence of contexts, stopping on the first failure
sequenceMigrations :: Monad m => [m (MigrationResult e)] -> m (MigrationResult e)
sequenceMigrations = \case
    []   -> return MigrationSuccess
    c:cs -> do
        r <- c
        case r of
          MigrationError s -> return (MigrationError s)
          MigrationSuccess -> sequenceMigrations cs

-- | Executes all SQL-file based migrations located in the provided 'dir'
-- in alphabetical order.
executeDirectoryMigration :: Connection -> BS.ByteString -> Bool -> FilePath -> IO (MigrationResult String)
executeDirectoryMigration con tableName verbose dir =
    scriptsInDirectory dir >>= go
    where
        go fs = sequenceMigrations (executeMigrationFile <$> fs)
        executeMigrationFile f = executeMigration con tableName verbose f =<< BS.readFile (dir ++ "/" ++ f)

-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [String]
scriptsInDirectory dir =
    fmap (sort . filter (\x -> not $ "." `isPrefixOf` x))
        (getDirectoryContents dir)

-- | Executes a generic SQL migration for the provided script 'name' with
-- content 'contents'.
executeMigration :: Connection -> BS.ByteString -> Bool -> ScriptName -> BS.ByteString -> IO (MigrationResult String)
executeMigration con tableName verbose name contents = do
    let checksum = md5Hash contents
    checkScript con tableName name checksum >>= \case
        ScriptOk -> do
            when verbose $ putStrLn $ "Ok:\t" ++ name
            return MigrationSuccess
        ScriptNotExecuted -> do
            void $ execute_ con (Query contents)
            void $ execute con q (name, checksum)
            when verbose $ putStrLn $ "Execute:\t" ++ name
            return MigrationSuccess
        ScriptModified { actual, expected } -> do
            when verbose $ putStrLn
              $ "Fail:\t" ++ name
              ++ "\n" ++ scriptModifiedErrorMessage expected actual
            return (MigrationError name)
    where
        q = "insert into " <> Query tableName <> "(filename, checksum) values(?, ?)"

-- | Initializes the database schema with a helper table containing
-- meta-information about executed migrations.
initializeSchema :: Connection -> BS.ByteString -> Bool -> IO ()
initializeSchema con tableName verbose = do
    when verbose $ putStrLn "Initializing schema"
    void $ execute_ con $ mconcat
        [ "create table if not exists " <> Query tableName <> " "
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
-- * 'MigrationCommands': validates all the sub-commands stopping at the first failure.
executeValidation :: Connection -> BS.ByteString -> Bool -> MigrationCommand -> IO (MigrationResult String)
executeValidation con tableName' verbose cmd = 
  let tableName = BS8.unpack tableName' in
  case cmd of
    MigrationInitialization ->
        existsTable con tableName >>= \r -> return $ if r
            then MigrationSuccess
            else MigrationError $ "No such table: " <> tableName
    MigrationDirectory path ->
        scriptsInDirectory path >>= goScripts path
    MigrationScript name contents ->
        validate name contents
    MigrationFile name path ->
        validate name =<< BS.readFile path
    MigrationValidation _ ->
        return MigrationSuccess
    MigrationCommands cs ->
        sequenceMigrations (executeValidation con tableName' verbose <$> cs)
    where
        validate name contents =
            checkScript con tableName' name (md5Hash contents) >>= \case
                ScriptOk -> do
                    when verbose $ putStrLn $ "Ok:\t" ++ name
                    return MigrationSuccess
                ScriptNotExecuted -> do
                    when verbose $ putStrLn $ "Missing:\t" ++ name
                    return (MigrationError $ "Missing: " ++ name)
                ScriptModified { expected, actual } -> do
                    when verbose $ putStrLn
                      $ "Checksum mismatch:\t" ++ name
                      ++ "\n" ++ scriptModifiedErrorMessage expected actual
                    return (MigrationError $ "Checksum mismatch: " ++ name)

        goScripts path xs = sequenceMigrations (goScript path <$> xs)
        goScript path x = validate x =<< BS.readFile (path ++ "/" ++ x)

-- | Checks the status of the script with the given name 'name'.
-- If the script has already been executed, the checksum of the script
-- is compared against the one that was executed.
-- If there is no matching script entry in the database, the script
-- will be executed and its meta-information will be recorded.
checkScript :: Connection -> BS.ByteString -> ScriptName -> Checksum -> IO CheckScriptResult
checkScript con tableName name fileChecksum =
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
            [ "select checksum from " <> Query tableName <> " "
            , "where filename = ? limit 1"
            ]

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
    mappend (MigrationCommands xs) (MigrationCommands ys) = MigrationCommands (xs ++ ys)
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

scriptModifiedErrorMessage :: Checksum -> Checksum -> [Char]
scriptModifiedErrorMessage expected actual =
  "expected: " ++ show expected ++ "\nhash was: " ++ show actual

-- | A sum-type denoting the result of a migration.
data MigrationResult a
    = MigrationError a
    -- ^ There was an error in script migration.
    | MigrationSuccess
    -- ^ All scripts have been executed successfully.
    deriving (Show, Eq, Read, Ord, Functor, Foldable, Traversable)

-- | The 'MigrationContext' provides an execution context for migrations.
data MigrationContext = MigrationContext
    { migrationContextCommand    :: !MigrationCommand
    -- ^ The action that will be performed by 'runMigration'
    , migrationContextVerbose    :: !Bool
    -- ^ Verbosity of the library.
    , migrationContextConnection :: !Connection
    -- ^ The PostgreSQL connection to use for migrations.
    }

-- | The 'MigrationContext'' provides an execution context for migrations, with additional options to MigrationContext
data MigrationContext' = MigrationContext'
    { migrationContextCommand'    :: !MigrationCommand
    -- ^ The action that will be performed by 'runMigration'
    , migrationContextVerbose'    :: !Bool
    -- ^ Verbosity of the library.
    , migrationContextConnection' :: !Connection
    -- ^ The PostgreSQL connection to use for migrations.
    , migrationTableName :: !BS.ByteString
    -- ^ The name of the table that stores the migrations
    }
--
-- | Produces a list of all executed 'SchemaMigration's.
getMigrations :: Connection -> IO [SchemaMigration]
getMigrations con = getMigrations' con "schema_migrations"

-- | Produces a list of all executed 'SchemaMigration's.
getMigrations' :: Connection -> BS.ByteString -> IO [SchemaMigration]
getMigrations' con tableName = query_ con q
    where q = mconcat
            [ "select filename, checksum, executed_at "
            , "from " <> Query tableName <> " order by executed_at asc"
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
