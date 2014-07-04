module Main (
    main
    ) where

import           Control.Monad                        (void)
import qualified Data.ByteString.Char8                as BS8 (pack)
import           Database.PostgreSQL.Simple           (connectPostgreSQL)
import           Database.PostgreSQL.Simple.Migration (MigrationContext (..),
                                                       initializeMigrations,
                                                       runMigrations)
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure)

main :: IO ()
main = getArgs >>= \args -> case args of
    "-h":_ ->
        printUsage
    "-q":xs ->
        run (parseCommand xs) False
    xs ->
        run (parseCommand xs) True

run :: Maybe MigrationCommand -> Bool-> IO ()
run Nothing  _ = printUsage >> exitFailure
run (Just cmd) verbose =
    void $ case cmd of
        Initialize url -> do
            con <- connectPostgreSQL (BS8.pack url)
            initializeMigrations (MigrationContext "" verbose con)
        Migrate url dir -> do
            con <- connectPostgreSQL (BS8.pack url)
            runMigrations (MigrationContext dir verbose con)

parseCommand :: [String] -> Maybe MigrationCommand
parseCommand ("init":url:_) = Just (Initialize url)
parseCommand ("migrate":url:dir:_) = Just (Migrate url dir)
parseCommand _ = Nothing

printUsage :: IO ()
printUsage = do
    putStrLn "migrate [options] <command>"
    putStrLn "  Options:"
    putStrLn "      -h          Print help text"
    putStrLn "      -q          Enable quiet mode"
    putStrLn "  Commands:"
    putStrLn "      init <url>"
    putStrLn "                  Initialize the database. Required to be run"
    putStrLn "                  at least once."
    putStrLn "      migrate <url> <directory>"
    putStrLn "                  Execute all SQL scripts in the provided"
    putStrLn "                  directory in alphabetical order."
    putStrLn "                  Scripts that have already been executed are"
    putStrLn "                  ignored. If a script was changed since the"
    putStrLn "                  time of its last execution, an error is"
    putStrLn "                  raised. The provided URL must have the"
    putStrLn "                  following syntax: "
    putStrLn "                  postgresql://$username:$password@$hostname:$port/$dbname"

data MigrationCommand
    = Initialize String
    | Migrate String String
    deriving (Show, Eq, Read, Ord)
