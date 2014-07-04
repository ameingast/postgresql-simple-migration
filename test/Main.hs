module Main
    ( main
    ) where

import           Database.PostgreSQL.Simple               (connectPostgreSQL,
                                                           rollback,
                                                           withTransaction)
import           Database.PostgreSQL.Simple.MigrationTest (migrationSpec)
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    con <- connectPostgreSQL "dbname=test"
    withTransaction con $ hspec (migrationSpec con) >> rollback con

