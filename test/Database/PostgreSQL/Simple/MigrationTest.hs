-- |
-- Module      : Database.PostgreSQL.Simple.MigrationTest
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of postgresql-simple-migration specifications.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Simple.MigrationTest where

import           Database.PostgreSQL.Simple           (Connection)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult (..),
                                                       SchemaMigration (..),
                                                       getMigrations,
                                                       runMigration)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           Test.Hspec                           (Spec, describe, it,
                                                       shouldBe)

migrationSpec:: Connection -> Spec
migrationSpec con = describe "Migrations" $ do
    let migrationScript = MigrationScript "test.sql" q
    let migrationScriptAltered = MigrationScript "test.sql" ""
    let migrationDir = MigrationDirectory "share/test/scripts"
    let migrationFile = MigrationFile "s.sql" "share/test/script.sql"

    it "asserts that the schema_migrations table does not exist" $ do
        r <- existsTable con "schema_migrations"
        r `shouldBe` False

    it "validates an initialization on an empty database" $ do
        r <- runMigration $ MigrationContext
            (MigrationValidation MigrationInitialization) False con
        r `shouldBe` MigrationError "No such table: schema_migrations"

    it "initializes a database" $ do
        r <- runMigration $ MigrationContext MigrationInitialization False con
        r `shouldBe` MigrationSuccess

    it "creates the schema_migrations table" $ do
        r <- existsTable con "schema_migrations"
        r `shouldBe` True

    it "executes a migration script" $ do
        r <- runMigration $ MigrationContext migrationScript False con
        r `shouldBe` MigrationSuccess

    it "creates the table from the executed script" $ do
        r <- existsTable con "t1"
        r `shouldBe` True

    it "skips execution of the same migration script" $ do
        r <- runMigration $
            MigrationContext migrationScript False con
        r `shouldBe` MigrationSuccess

    it "reports an error on a different checksum for the same script" $ do
        r <- runMigration $ MigrationContext migrationScriptAltered False con
        r `shouldBe` MigrationError "test.sql"

    it "executes migration scripts inside a folder" $ do
        r <- runMigration $ MigrationContext migrationDir False con
        r `shouldBe` MigrationSuccess

    it "creates the table from the executed scripts" $ do
        r <- existsTable con "t2"
        r `shouldBe` True

    it "executes a file based migration script" $ do
        r <- runMigration $ MigrationContext migrationFile False con
        r `shouldBe` MigrationSuccess

    it "creates the table from the executed scripts" $ do
        r <- existsTable con "t3"
        r `shouldBe` True

    it "validates initialization" $ do
        r <- runMigration $ MigrationContext
            (MigrationValidation MigrationInitialization) False con
        r `shouldBe` MigrationSuccess

    it "validates an executed migration script" $ do
        r <- runMigration $ MigrationContext
            (MigrationValidation migrationScript) False con
        r `shouldBe` MigrationSuccess

    it "validates all scripts inside a folder" $ do
        r <- runMigration $ MigrationContext
            (MigrationValidation migrationDir) False con
        r `shouldBe` MigrationSuccess

    it "validates an executed migration file" $ do
        r <- runMigration $ MigrationContext
            (MigrationValidation migrationFile) False con
        r `shouldBe` MigrationSuccess

    it "gets a list of executed migrations" $ do
        r <- getMigrations con
        map schemaMigrationName r `shouldBe` ["test.sql", "1.sql", "s.sql"]

    where
        q = "create table t1 (c1 varchar);"

