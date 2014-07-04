{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Simple.MigrationTest where

import           Control.Monad                        (liftM)
import           Database.PostgreSQL.Simple           (Connection, Only (..),
                                                       query)
import           Database.PostgreSQL.Simple.Migration
import           Test.Hspec                           (Spec, describe, it,
                                                       shouldBe)

migrationSpec:: Connection -> Spec
migrationSpec con = describe "runMigration" $ do
    it "initializes a database" $ do
        r <- runMigration $
            MigrationContext MigrationInitialization False con
        r `shouldBe` MigrationSuccess

    it "creates the schema_migration table" $ do
        r <- existsTable con "schema_migration"
        r `shouldBe` True

    it "executes a migration script" $ do
        r <- runMigration $
            MigrationContext (MigrationScript "test.sql" t1) False con
        r `shouldBe` MigrationSuccess

    it "creates the table from the executed script" $ do
        r <- existsTable con "t1"
        r `shouldBe` True

    it "skips execution of the same migration script" $ do
        r <- runMigration $
            MigrationContext (MigrationScript "test.sql" t1) False con
        r `shouldBe` MigrationSuccess

    it "reports an error on a different checksum for the same script" $ do
        r <- runMigration $
            MigrationContext (MigrationScript "test.sql" "") False con
        r `shouldBe` MigrationError "test.sql"
    where
        t1 = "create table t1 (c1 varchar);"

existsTable :: Connection -> String -> IO Bool
existsTable con table =
    liftM (not . null) (query con q (Only table) :: IO [[Int]])
    where
        q = "select count(relname) from pg_class where relname = ?"
