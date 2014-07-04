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


