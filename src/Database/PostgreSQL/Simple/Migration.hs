-- |
-- Module      : Database.PostgreSQL.Simple.Migration
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Migration library for postgresql-simple.

module Database.PostgreSQL.Simple.Migration
    ( runMigration
    , MigrationContext(..)
    , MigrationCommand(..)
    , MigrationResult(..)
    , ScriptName
    ) where

import           Control.Monad                    (liftM, void, when)
import qualified Crypto.Hash.MD5                  as MD5 (hash)
import qualified Data.ByteString                  as BS (ByteString, readFile)
import qualified Data.ByteString.Base64           as B64 (encode)
import           Data.List                        (isPrefixOf, sort)
import           Data.Monoid                      (mconcat)
import           Database.PostgreSQL.Simple       (Connection, Only (..),
                                                   execute, execute_, query)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           System.Directory                 (getDirectoryContents)

-- | Executes migrations inside the provided 'MigrationContext'.
runMigration :: MigrationContext -> IO (MigrationResult String)
runMigration (MigrationContext cmd verbose con) = case cmd of
    MigrationInitialization ->
        initializeSchema con verbose >> return MigrationSuccess
    MigrationDirectory path ->
       executeDirectoryMigration con verbose path
    MigrationScript name contents ->
        executeMigration con verbose name contents
    MigrationFile name path ->
        executeMigration con verbose name =<< BS.readFile path

-- | Executes all SQL-file based migrations located in the provided 'dir'.
executeDirectoryMigration :: Connection -> Bool -> FilePath -> IO (MigrationResult String)
executeDirectoryMigration con verbose dir =
    liftM (filter (\x -> not $ "." `isPrefixOf` x))
        (getDirectoryContents dir) >>= go . sort
    where
        go [] = return MigrationSuccess
        go (f:fs) = do
            r <- executeMigration con verbose f =<< BS.readFile (dir ++ "/" ++ f)
            case r of
                MigrationError _ ->
                    return r
                MigrationSuccess ->
                    go fs

-- | Executes a generic SQL migration for the provided script 'name' with
-- content 'contents'.
executeMigration :: Connection -> Bool -> ScriptName -> BS.ByteString -> IO (MigrationResult String)
executeMigration con verbose name contents = do
    let checksum = md5Hash contents
    checkScript con name checksum >>= \r -> case r of
        ScriptOk -> do
            when verbose $ putStrLn $ "Ok:\t" ++ name
            return MigrationSuccess
        ScriptNotExecuted -> do
            void $ execute_ con (Query contents)
            void $ execute con q (name, checksum)
            when verbose $ putStrLn $ "Execute:\t" ++ name
            return MigrationSuccess
        ScriptModified _ -> do
            when verbose $ putStrLn $ "Fail:\t" ++ name
            return (MigrationError name)
    where
        q = "insert into schema_migrations(filename, checksum) values(?, ?) "

-- | Initializes the database schema with a helper table containing
-- meta-information about executed migrations.
initializeSchema :: Connection -> Bool -> IO ()
initializeSchema con verbose = do
    when verbose $ putStrLn "Initializing schema"
    void $ execute_ con $ mconcat
        [ "create table if not exists schema_migrations "
        , "( filename varchar(512) not null"
        , ", checksum varchar(32) not null"
        , ", executed_at timestamp without time zone not null default now() "
        , ");"
        ]

-- | Checks the status of the script with the given name 'name'.
-- If the script has already been executed, the checksum of the script
-- is compared against the one that was executed.
-- If there is no matching script entry in the database, the script
-- will be executed and its meta-information will be recorded.
checkScript :: Connection -> ScriptName -> Checksum -> IO CheckScriptResult
checkScript con name checksum =
    query con q (Only name) >>= \r -> case r of
        [] ->
            return ScriptNotExecuted
        Only actualChecksum:_ | checksum == actualChecksum ->
            return ScriptOk
        Only actualChecksum:_ ->
            return (ScriptModified actualChecksum)
    where
        q = mconcat
            [ "select checksum from schema_migrations "
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
    deriving (Show, Eq, Read, Ord)

-- | A sum-type denoting the result of a single migration.
data CheckScriptResult
    = ScriptOk
    -- ^ The script has already been executed and the checksums match.
    -- This is good.
    | ScriptModified Checksum
    -- ^ The script has already been executed and there is a checksum
    -- mismatch. This is bad.
    | ScriptNotExecuted
    -- ^ The script has not been executed, yet. This is good.
    deriving (Show, Eq, Read, Ord)

-- | A sum-type denoting the result of a migration.
data MigrationResult a
    = MigrationError a
    -- ^ There was an error in script migration.
    | MigrationSuccess
    -- ^ All scripts have been executed successfully.
    deriving (Show, Eq, Read, Ord)

-- | The 'MigrationContext' provides an execution context for migrations.
data MigrationContext = MigrationContext
    { migrationContextCommand    :: MigrationCommand
    -- ^ The action that will be performed by 'runMigration'
    , migrationContextVerbose    :: Bool
    -- ^ Verbosity of the library.
    , migrationContextConnection :: Connection
    -- ^ The PostgreSQL connection to use for migrations.
    }
