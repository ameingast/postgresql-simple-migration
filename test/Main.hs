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

module Main
    ( main
    ) where

import           Control.Exception                        (finally)
import           Database.PostgreSQL.Simple               (begin,
                                                           connectPostgreSQL,
                                                           rollback)
import           Database.PostgreSQL.Simple.MigrationTest (migrationSpec)
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    con <- connectPostgreSQL "dbname=test"
    begin con
    finally
        (hspec (migrationSpec con))
        (rollback con)


