module Main (
    main
    ) where

import           Control.Monad                        (void)
import qualified Data.ByteString.Char8                as BS8 (pack)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           System.Environment                   (getArgs)

main :: IO ()
main = getArgs >>= \args -> case args of
    "-h":_ ->
        printUsage
    "-c":url:dir:_-> do
        con <- connectPostgreSQL (BS8.pack url)
        void $ runMigrations MigrationContext
            { migrationContextBasePath = dir
            , migrationContextConnection = con
            , migrationContextVerbose = True
            }
    _ ->
        printUsage

printUsage :: IO ()
printUsage = do
    putStrLn "migrate [options] <directory>"
    putStrLn "  Options:"
    putStrLn "      -h          Print help text"
    putStrLn "      -c <url>    PostgreSQL connection url in format:"
    putStrLn "                  postgresql://$username:$password@$hostname:$port/$dbname"


