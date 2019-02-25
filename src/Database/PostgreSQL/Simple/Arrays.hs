{-# LANGUAGE PatternGuards #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Arrays
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- A Postgres array parser and pretty-printer.
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Arrays where

import           Control.Applicative (Applicative(..), Alternative(..), (<$>))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           Data.Attoparsec.ByteString.Char8


-- | Parse one of three primitive field formats: array, quoted and plain.
arrayFormat :: Char -> Parser ArrayFormat
arrayFormat delim  =  Array  <$> array delim
                  <|> Plain  <$> plain delim
                  <|> Quoted <$> quoted

data ArrayFormat = Array [ArrayFormat]
                 | Plain ByteString
                 | Quoted ByteString
                   deriving (Eq, Show, Ord)

array :: Char -> Parser [ArrayFormat]
array delim = char '{' *> option [] (arrays <|> strings) <* char '}'
  where
    strings = sepBy1 (Quoted <$> quoted <|> Plain <$> plain delim) (char delim)
    arrays  = sepBy1 (Array <$> array delim) (char ',')
    -- NB: Arrays seem to always be delimited by commas.

-- | Recognizes a quoted string.
quoted :: Parser ByteString
quoted  = char '"' *> option "" contents <* char '"'
  where
    esc' = char '\\' *> (char '\\' <|> char '"')
    unQ = takeWhile1 (notInClass "\"\\")
    contents = mconcat <$> many (unQ <|> B.singleton <$> esc')

-- | Recognizes a plain string literal, not containing quotes or brackets and
--   not containing the delimiter character.
plain :: Char -> Parser ByteString
plain delim = takeWhile1 (notInClass (delim:"\"{}"))

-- Mutually recursive 'fmt' and 'delimit' separate out value formatting
-- from the subtleties of delimiting.

-- | Format an array format item, using the delimiter character if the item is
--   itself an array.
fmt :: Char -> ArrayFormat -> ByteString
fmt = fmt' False

-- | Format a list of array format items, inserting the appropriate delimiter
--   between them. When the items are arrays, they will be delimited with
--   commas; otherwise, they are delimited with the passed-in-delimiter.
delimit :: Char -> [ArrayFormat] -> ByteString
delimit _      [] = ""
delimit c     [x] = fmt' True c x
delimit c (x:y:z) = (fmt' True c x `B.snoc` c') `mappend` delimit c (y:z)
  where
    c' | Array _ <- x = ','
       | otherwise    = c

-- | Format an array format item, using the delimiter character if the item is
--   itself an array, optionally applying quoting rules. Creates copies for
--   safety when used in 'FromField' instances.
fmt' :: Bool -> Char -> ArrayFormat -> ByteString
fmt' quoting c x =
  case x of
    Array items          -> '{' `B.cons` (delimit c items `B.snoc` '}')
    Plain bytes          -> B.copy bytes
    Quoted q | quoting   -> '"' `B.cons` (esc q `B.snoc` '"')
             | otherwise -> B.copy q
    -- NB: The 'snoc' and 'cons' functions always copy.

-- | Escape a string according to Postgres double-quoted string format.
esc :: ByteString -> ByteString
esc = B.concatMap f
  where
    f '"'  = "\\\""
    f '\\' = "\\\\"
    f c    = B.singleton c
  -- TODO: Implement easy performance improvements with unfoldr.
