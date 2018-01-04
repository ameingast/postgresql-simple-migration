-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A standalone program for the postgresql-simple-migration library.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Exception
import qualified Data.ByteString.Char8                as BS8 (pack)
import           Data.Time
import           Database.PostgreSQL.Simple           (SqlError (..),
                                                       connectPostgreSQL,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult (..),
                                                       runMigration)
import           System.Directory                     (withCurrentDirectory)
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure, exitSuccess)
import           System.IO                            (writeFile)

import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T

main :: IO ()
main = getArgs >>= \case
    "-h":_ ->
        printUsage
    "-q":xs ->
        ppException $ run (parseCommand xs) False
    xs ->
        ppException $ run (parseCommand xs) True

-- | Pretty print postgresql-simple exceptions to see whats going on
ppException :: IO a -> IO a
ppException a = catch a ehandler
  where
    ehandler e = maybe (throw e) (*> exitFailure)
                 (pSqlError <$> fromException e)
    bsToString = T.unpack . T.decodeUtf8
    pSqlError e = mapM_ putStrLn
                  [ "SqlError:"
                  , "  sqlState: "
                  , bsToString $ sqlState e
                  , "  sqlExecStatus: "
                  , show $ sqlExecStatus e
                  , "  sqlErrorMsg: "
                  , bsToString $ sqlErrorMsg e
                  , "  sqlErrorDetail: "
                  , bsToString $ sqlErrorDetail e
                  , "  sqlErrorHint: "
                  , bsToString $ sqlErrorHint e
                  ]

run :: Maybe Command -> Bool-> IO ()
run Nothing  _ = printUsage >> exitFailure
run (Just cmd) verbose =
    handleResult =<< case cmd of
        Initialize url -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration $ MigrationContext
                MigrationInitialization verbose con
        Create name dir -> do
            now <- getZonedTime
            let filePath = (formatTime defaultTimeLocale "%Y%m%d%H%M%S" now)
                  ++ "_" ++ name ++ ".sql"
            putStrLn filePath
            withCurrentDirectory dir $ writeFile filePath ""
            pure MigrationSuccess
        Migrate url dir -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration $ MigrationContext
                (MigrationDirectory dir) verbose con
        Validate url dir -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration $ MigrationContext
                (MigrationValidation (MigrationDirectory dir)) verbose con
    where
        handleResult MigrationSuccess   = exitSuccess
        handleResult (MigrationError _) = exitFailure

parseCommand :: [String] -> Maybe Command
parseCommand ("init":url:_)         = Just (Initialize url)
parseCommand ("create":name:dir:_)  = Just (Create name dir)
parseCommand ("migrate":url:dir:_)  = Just (Migrate url dir)
parseCommand ("validate":url:dir:_) = Just (Validate url dir)
parseCommand _                      = Nothing

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
    putStrLn "      create <name> <directory>"
    putStrLn "                  Create skeleton SQL script with name,"
    putStrLn "                  prefixed by timestamp to ensure the order."
    putStrLn "      migrate <con> <directory>"
    putStrLn "                  Execute all SQL scripts in the provided"
    putStrLn "                  directory in alphabetical order."
    putStrLn "                  Scripts that have already been executed are"
    putStrLn "                  ignored. If a script was changed since the"
    putStrLn "                  time of its last execution, an error is"
    putStrLn "                  raised."
    putStrLn "      validate <con> <directory>"
    putStrLn "                  Validate all SQL scripts in the provided"
    putStrLn "                  directory."
    putStrLn "      The <con> parameter is based on libpq connection string"
    putStrLn "      syntax. Detailled information is available here:"
    putStrLn "      <http://www.postgresql.org/docs/9.3/static/libpq-connect.html>"

data Command
    = Initialize String
    | Create String FilePath
    | Migrate String FilePath
    | Validate String FilePath
    deriving (Show, Eq, Read, Ord)
