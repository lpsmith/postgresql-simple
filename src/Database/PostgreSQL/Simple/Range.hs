{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.PostgreSQL.Simple.Range
       where

import           Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import           Blaze.ByteString.Builder.Char8    (fromChar)
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8  (Parser)
import qualified Data.Attoparsec.ByteString.Char8  as A
import qualified Data.ByteString                   as B
import           Data.Monoid                       (mempty)
import           Data.Typeable                     (Typeable)

import           Database.PostgreSQL.Simple.Compat ((<>))

-- | Represents boundary of a range
data RangeBound a = Unbounded
                  | Inclusive a
                  | Exclusive a
     deriving (Show, Typeable, Eq, Ord, Functor)

-- | Generic range type
data PGRange a = PGRange (RangeBound a) (RangeBound a)
     deriving (Show, Typeable, Eq, Ord, Functor)

lowerBound :: Parser (a -> RangeBound a)
lowerBound = (A.char '(' *> pure Exclusive) <|> (A.char '[' *> pure Inclusive)
{-# INLINE lowerBound #-}

upperBound :: Parser (a -> RangeBound a)
upperBound = (A.char ')' *> pure Exclusive) <|> (A.char ']' *> pure Inclusive)
{-# INLINE upperBound #-}

-- | Generic range parser
pgrange :: Parser (RangeBound B.ByteString, RangeBound B.ByteString)
pgrange = do
  lb <- lowerBound
  v1 <- (A.char ',' *> "") <|> (rangeElem (==',') <* A.char ',')
  v2 <- rangeElem $ \c -> c == ')' || c == ']'
  ub <- upperBound
  A.endOfInput
  let low = if B.null v1 then Unbounded else lb v1
      up  = if B.null v2 then Unbounded else ub v2
  return (low, up)

rangeElem :: (Char -> Bool) -> Parser B.ByteString
rangeElem end = (A.char '"' *> doubleQuoted)
            <|> A.takeTill end
{-# INLiNE rangeElem #-}

-- | Simple double quoted value parser
doubleQuoted :: Parser B.ByteString
doubleQuoted = toByteString <$> go mempty
  where
    go acc = do
      h <- fromByteString <$> A.takeTill (\c -> c == '\\' || c == '"')
      let rest = do
           start <- A.anyChar
           case start of
             '\\' -> do
               c <- A.anyChar
               go (acc <> h <> fromChar c)
             '"' -> (A.char '"' *> go (acc <> h <> fromChar '"'))
                    <|> pure (acc <> h)
             _ -> error "impossible in doubleQuoted"
      rest

-- | Generic range to builder for plain values
rangeToBuilder :: (a -> Builder) -> PGRange a -> Builder
rangeToBuilder _ (PGRange Unbounded Unbounded) = fromByteString "'empty'"
rangeToBuilder f (PGRange a b) = buildLB a <> buildUB b
  where
    buildLB Unbounded = fromByteString "'[,"
    buildLB (Inclusive v) = fromByteString "'[\"" <> f v <> fromByteString "\","
    buildLB (Exclusive v) = fromByteString "'(\"" <> f v <> fromByteString "\","

    buildUB Unbounded = fromByteString "]'"
    buildUB (Inclusive v) = fromChar '"' <> f v <> fromByteString "\"]'"
    buildUB (Exclusive v) = fromChar '"' <> f v <> fromByteString "\")'"
{-# INLINE rangeToBuilder #-}

