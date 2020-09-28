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
import           Database.PostgreSQL.Simple           (SqlError (..),
                                                       connectPostgreSQL,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult (..),
                                                       runMigration)
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure, exitSuccess)
import           System.IO                            (Handle, hPutStrLn,
                                                       stdout, stderr)

import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T

main :: IO ()
main = getArgs >>= \case
    x:_  | x `elem` ["-h", "--help"] ->
        printUsage stdout
    x:xs | x `elem` ["-q", "--quiet"] ->
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
    pSqlError e = mapM_ (hPutStrLn stderr)
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

run :: Maybe Command -> Bool -> IO ()
run Nothing  _ = printUsage stderr >> exitFailure
run (Just cmd) verbose =
    handleResult =<< case cmd of
        Initialize url -> do
            con <- connectPostgreSQL (BS8.pack url)
            withTransaction con $ runMigration $ MigrationContext
                MigrationInitialization verbose con
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
parseCommand ("migrate":url:dir:_)  = Just (Migrate url dir)
parseCommand ("validate":url:dir:_) = Just (Validate url dir)
parseCommand _                      = Nothing

printUsage :: Handle -> IO ()
printUsage h = do
    say "migrate [options] <command>"
    say "  Options:"
    say "      -h --help   Print help text"
    say "      -q --quiet  Enable quiet mode"
    say "  Commands:"
    say "      init <con>"
    say "                  Initialize the database. Required to be run"
    say "                  at least once."
    say "      migrate <con> <directory>"
    say "                  Execute all SQL scripts in the provided"
    say "                  directory in alphabetical order."
    say "                  Scripts that have already been executed are"
    say "                  ignored. If a script was changed since the"
    say "                  time of its last execution, an error is"
    say "                  raised."
    say "      validate <con> <directory>"
    say "                  Validate all SQL scripts in the provided"
    say "                  directory."
    say "      The <con> parameter is based on libpq connection string"
    say "      syntax. Detailled information is available here:"
    say "      <http://www.postgresql.org/docs/9.3/static/libpq-connect.html>"
    where
        say = hPutStrLn h

data Command
    = Initialize String
    | Migrate String FilePath
    | Validate String FilePath
    deriving (Show, Eq, Read, Ord)
