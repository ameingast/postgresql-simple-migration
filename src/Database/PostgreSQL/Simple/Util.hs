-- |
-- Module      : Database.PostgreSQL.Simple.Util
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of utilites for database migrations.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Simple.Util
    ( existsTable
    , withTransactionRolledBack
    ) where

import           Control.Exception.Safe     (finally)
import           Database.PostgreSQL.Simple (Connection, Only (..), begin,
                                             query, rollback)

-- | Checks if the table with the given name exists in the database.
existsTable :: Connection -> String -> IO Bool
existsTable con table =
    fmap (not . null) (query con q (Only table) :: IO [[Int]])
    where
        q = "select count(relname) from pg_class where relname = ?"

-- | Executes the given IO monad inside a transaction and performs a roll-back
-- afterwards (even if exceptions occur).
withTransactionRolledBack :: Connection -> IO a -> IO a
withTransactionRolledBack con f =
    begin con >> finally f (rollback con)
