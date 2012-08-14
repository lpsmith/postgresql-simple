{-# LANGUAGE PatternGuards #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Arrays
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A Postgres array parser and pretty-printer.
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Arrays where

import           Control.Applicative (Applicative(..), Alternative(..), (<$>))
import           Data.ByteString.Char8 (ByteString, snoc, cons, copy)
import           Data.Monoid
import           Data.Attoparsec.Char8
import           Database.PostgreSQL.Simple.FromField


-- | Parse one of three primitive field formats: array, quoted and plain.
arrayFormat :: Char -> Parser ArrayFormat
arrayFormat delim  =  Array <$> bracketed delim
                  <|> Bytes <$> plain delim
                  <|> Bytes <$> quoted

data ArrayFormat = Array [ArrayFormat] | Bytes ByteString
    deriving (Eq, Show, Ord)

bracketed :: Char -> Parser [ArrayFormat]
bracketed delim = char '{' *> option [] (arrays <|> strings) <* char '}'
  where
    strings = sepBy1 (Bytes <$> (quoted <|> plain delim)) (char delim)
    arrays  = sepBy1 (Array <$> bracketed delim) (char ',')
    -- NB: Arrays seem to always be delimited by commas.

-- | Recognizes a quoted string. Doesn't parse it in the sense that no
--   extraction occurs -- valid escape sequences and the delimiting quotes are
--   left in place. Makes a copy, because 'snoc' and 'cons' are used.
quoted :: Parser ByteString
quoted  = snoc <$> (cons <$> char '"' <*> option "" contents) <*> char '"'
  where
    esc = string "\\\\" <|> string "\\\""
    unQ = takeWhile1 (notInClass "\"\\")
    contents = mconcat <$> many (esc <|> unQ)

-- | Recognizes a plain string literal, not containing quotes or brackets and
--   not containing the delimiter character. Makes a copy, to be consistent
--   with 'quoted', above.
plain :: Char -> Parser ByteString
plain delim = copy <$> takeWhile1 (notInClass (delim:"\"{}"))

-- Mutually recursive 'fmt' and 'delimit' separate out value formatting
-- from the subtleties of delimiting.

-- | Format an array format item, using the delimiter character if the item is
--   itself an array.
fmt :: Char -> ArrayFormat -> ByteString
fmt c x =
  case x of
    Array items -> '{' `cons` delimit c items `snoc` '}'
    Bytes bytes -> bytes

-- | Format a list of array format items, inserting the appropriate delimiter
--   between them. When the items are arrays, they will be delimited with
--   commas; otherwise, they are delimited with the passed-in-delimiter.
delimit :: Char -> [ArrayFormat] -> ByteString
delimit _           [] = ""
delimit c          [x] = fmt c x
delimit c (x:next:rem) = fmt c x `snoc` c' `mappend` delimit c (next:rem)
  where
    c' | Array _ <- x = ','
       | otherwise    = c

