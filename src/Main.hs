module Main (
    main
    ) where

import           Control.Monad                        (void)
import qualified Data.ByteString.Char8                as BS8 (pack)
import           Database.PostgreSQL.Simple           (connectPostgreSQL)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       runMigration)
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

run :: Maybe Command -> Bool-> IO ()
run Nothing  _ = printUsage >> exitFailure
run (Just cmd) verbose =
    void $ case cmd of
        Initialize url -> do
            con <- connectPostgreSQL (BS8.pack url)
            runMigration $ MigrationContext MigrationInitialization verbose con
        Migrate url dir -> do
            con <- connectPostgreSQL (BS8.pack url)
            runMigration $ MigrationContext (MigrationDirectory dir) verbose con

parseCommand :: [String] -> Maybe Command
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
    putStrLn "      init <con>"
    putStrLn "                  Initialize the database. Required to be run"
    putStrLn "                  at least once."
    putStrLn "      migrate <con> <directory>"
    putStrLn "                  Execute all SQL scripts in the provided"
    putStrLn "                  directory in alphabetical order."
    putStrLn "                  Scripts that have already been executed are"
    putStrLn "                  ignored. If a script was changed since the"
    putStrLn "                  time of its last execution, an error is"
    putStrLn "                  raised."
    putStrLn "      The <con> parameter is based on libpq connection string"
    putStrLn "      syntax. Detailled information is available here:"
    putStrLn "      <http://www.postgresql.org/docs/9.3/static/libpq-connect.html>"

data Command
    = Initialize String
    | Migrate String String
    deriving (Show, Eq, Read, Ord)
