-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The test entry-point for postgresql-simple-migration.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Database.PostgreSQL.Simple               (connectPostgreSQL)
import           Database.PostgreSQL.Simple.MigrationTest (migrationSpec)
import           Database.PostgreSQL.Simple.Util          (withTransactionRolledBack)
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    con <- connectPostgreSQL "dbname=test"
    withTransactionRolledBack con (hspec (migrationSpec con))
