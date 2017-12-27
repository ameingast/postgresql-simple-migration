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

{-# LANGUAGE QuasiQuotes #-}

module Database.PostgreSQL.Simple.ParserTest where

import           Control.Applicative               ((<$>))
import           Data.ByteString                   (ByteString)
import           Data.Monoid                       (mconcat)
import           Data.String.QQ
import           Database.PostgreSQL.Simple.Parser (toQueries)
import           Database.PostgreSQL.Simple.Types  (Query (..))
import           Test.Hspec                        (Spec, describe, it,
                                                    shouldBe)

query1, query2, query3, complexQ1, complexQ2, finalQuery :: ByteString
query1 = [s|CREATE TABLE test (test_item TEXT);|]
query2 = [s|INSERT INTO test values ('testing');|]
query3 = [s|DROP TABLE test;|]
complexQ1 = [s|INSERT INTO TABLE test values
                 ('Some;Semicolons;inside;quotes;');|]
complexQ2 = [s|INSERT INTO TABLE test values
                 ('Semicolons;\'inside\';a;complex;quote');|]
finalQuery = [s|SELECT 2 + 2 AS basic|]


parserSpec :: Spec
parserSpec = describe "Parser" $ do
  it "Will separate queries based on semicolon" $
    let q = toQueries (mconcat [query1, query2, query3])
    in length q `shouldBe` 3

  it "Will parse complete queries in the right order" $
    let q = toQueries (mconcat [query1, query2, query3])
    in q `shouldBe` (Query <$> [query1, query2, query3])

  it "Will not separate queries on semicolons inside quotes" $
    let q = toQueries (mconcat [query1, complexQ1, query2])
    in length q `shouldBe` 3

  it "Will understand denormalized quotes" $
    let q = toQueries (mconcat [query1, complexQ2, query2])
    in length q `shouldBe` 3
  it "Will be able to process a final query without a semicolon" $
    let q = toQueries (mconcat [query1, query2, finalQuery])
    in q `shouldBe` (Query <$> [query1, query2, finalQuery])
