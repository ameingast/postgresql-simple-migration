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

{-# LANGUAGE RecordWildCards #-}

module Database.PostgreSQL.Simple.Parser
  (
    toQueries
  ) where

import qualified Data.ByteString                  as BS (ByteString, foldl)
import qualified Data.ByteString.Builder          as BSB
import qualified Data.ByteString.Lazy             as BSI (ByteString, toStrict)
import           Data.Monoid                      (mempty, (<>))
import           Data.Word
import           Database.PostgreSQL.Simple.Types (Query (..))

backslash, singleQuote, semicolon, lineFeed, carriageReturn :: Word8
backslash = 0x5c
singleQuote = 0x27
semicolon = 0x3b
lineFeed = 0x0a
carriageReturn = 0x0d

data ParsingContext = InQuote
                    | Escaping
                    | OutQuote

data ParsedQueries =
  Parsed { current  :: BSB.Builder
         , previous :: [BSI.ByteString] }

addToCurrent :: Word8 -> ParsedQueries -> ParsedQueries
addToCurrent c p@Parsed{..} = p { current = current <> BSB.word8 c }

finishCurrent :: ParsedQueries -> ParsedQueries
finishCurrent p@Parsed{..} =
  let builded = BSB.toLazyByteString current
  in if builded == mempty
     then p
     else p { current = mempty
            , previous = BSB.toLazyByteString current:previous }

type ParsingState = (ParsingContext, ParsedQueries)

-- | Cut a migration file into a series of queries so they can be isolated and
-- handled sequentially, helping identify which one failed if any.
toQueries :: BS.ByteString -> [Query]
toQueries =
  let parsed = BS.foldl parseChar (OutQuote, Parsed mempty [])
      queries = reverse . previous . finishCurrent . snd . parsed
  in fmap (Query . BSI.toStrict) . queries

parseChar :: ParsingState -> Word8 -> ParsingState
parseChar (InQuote, p) c
  | c == backslash = (Escaping, addToCurrent c p)
  | c == singleQuote = (OutQuote, addToCurrent c p)
  | otherwise = (InQuote, addToCurrent c p)
parseChar (Escaping, p) c = (InQuote, addToCurrent c p)
parseChar (OutQuote, p) c
  | c == singleQuote = (InQuote, addToCurrent c p)
  | c == semicolon = (OutQuote, finishCurrent . addToCurrent semicolon $ p)
  | c == semicolon = (OutQuote, finishCurrent . addToCurrent semicolon $ p)
  | c `elem` [lineFeed, carriageReturn] = (OutQuote, p)
  | otherwise = (OutQuote, addToCurrent c p)
