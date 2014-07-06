-- |
-- Module      : Database.PostgreSQL.Simple.Migration
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : ameingast@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Migration library for postgresql-simple.

module Database.PostgreSQL.Simple.Internal.Util
    ( existsTable
    ) where

import           Control.Monad              (liftM)
import           Database.PostgreSQL.Simple (Connection, Only (..), query)

-- | Checks if the table named 'table' exists in the provided database.
existsTable :: Connection -> String -> IO Bool
existsTable con table =
    liftM (not . null) (query con q (Only table) :: IO [[Int]])
    where
        q = "select count(relname) from pg_class where relname = ?"
