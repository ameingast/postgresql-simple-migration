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

module Database.PostgreSQL.Simple.Util
    ( existsTable
    ) where

import           Control.Monad              (liftM)
import           Database.PostgreSQL.Simple (Connection, Only (..), query)

-- | Checks if the table with the given name exists in the database.
existsTable :: Connection -> String -> IO Bool
existsTable con table =
    liftM (not . null) (query con q (Only table) :: IO [[Int]])
    where
        q = "select count(relname) from pg_class where relname = ?"
