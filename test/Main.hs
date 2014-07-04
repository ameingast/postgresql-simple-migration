module Main
    ( main
    ) where

import           Control.Exception                        (finally)
import           Database.PostgreSQL.Simple               (connectPostgreSQL,
                                                           rollback,
                                                           withTransaction)
import           Database.PostgreSQL.Simple.MigrationTest (migrationSpec)
import           Test.Hspec                               (hspec)

main :: IO ()
main = do
    con <- connectPostgreSQL "dbname=test"
    withTransaction con $ finally
        (hspec (migrationSpec con))
        (rollback con)


