{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Range
-- Copyright:   (c) 2014-2015 Leonid Onokhov
--              (c) 2014-2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Range
      ( RangeBound(..)
      , PGRange(..)
      , empty
      , isEmpty, isEmptyBy
      , contains, containsBy
      ) where

import GHC.Stack
import           Control.Applicative hiding (empty)
import           Data.Attoparsec.ByteString.Char8     (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString.Char8     as A
import qualified Data.ByteString                      as B
import           Data.ByteString.Builder
                   ( Builder, byteString, lazyByteString, char8
                   , intDec, int8Dec, int16Dec, int32Dec, int64Dec, integerDec
                   , wordDec, word8Dec, word16Dec, word32Dec, word64Dec
                   , doubleDec, floatDec )
import           Data.Int                             (Int16, Int32, Int64,
                                                       Int8)
import           Data.Function (on)
import           Data.Monoid                          (mempty)
import           Data.Scientific                      (Scientific)
import qualified Data.Text.Lazy.Builder               as LT
import qualified Data.Text.Lazy.Encoding              as LT
import           Data.Time                            (Day, LocalTime,
                                                       NominalDiffTime,
                                                       TimeOfDay, UTCTime,
                                                       ZonedTime,
                                                       zonedTimeToUTC)
import           Data.Typeable                        (Typeable)
import           Data.Word                            (Word, Word16, Word32,
                                                       Word64, Word8)

import           Database.PostgreSQL.Simple.Compat    (scientificBuilder, (<>), toByteString)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Time
                   hiding (PosInfinity, NegInfinity)
-- import qualified Database.PostgreSQL.Simple.Time as Time
import           Database.PostgreSQL.Simple.ToField

-- | Represents boundary of a range
data RangeBound a = NegInfinity
                  | Inclusive !a
                  | Exclusive !a
                  | PosInfinity
     deriving (Show, Typeable, Eq, Functor)

-- | Generic range type
data PGRange a = PGRange !(RangeBound a) !(RangeBound a)
     deriving (Show, Typeable, Functor)

empty :: (HasCallStack) => PGRange a
empty = PGRange PosInfinity NegInfinity

instance Ord a => Eq (PGRange a) where
  x == y = eq x y || (isEmpty x && isEmpty y)
   where eq (PGRange a m) (PGRange b n) = a == b && m == n

isEmptyBy :: (HasCallStack) => (a -> a -> Ordering) -> PGRange a -> Bool
isEmptyBy cmp v =
    case v of
      (PGRange PosInfinity _) -> True
      (PGRange _ NegInfinity) -> True
      (PGRange NegInfinity _) -> False
      (PGRange _ PosInfinity) -> False
      (PGRange (Inclusive x) (Inclusive y)) -> cmp x y == GT
      (PGRange (Inclusive x) (Exclusive y)) -> cmp x y /= LT
      (PGRange (Exclusive x) (Inclusive y)) -> cmp x y /= LT
      (PGRange (Exclusive x) (Exclusive y)) -> cmp x y /= LT

-- | Is a range empty?   If this returns 'True',  then the 'contains'
--   predicate will always return 'False'.   However,  if this returns
--   'False', it is not necessarily true that there exists a point for
--   which 'contains' returns 'True'.
--   Consider @'PGRange' ('Excludes' 2) ('Excludes' 3) :: PGRange Int@,
--   for example.
isEmpty :: (HasCallStack, Ord a) => PGRange a -> Bool
isEmpty = isEmptyBy compare


-- | Does a range contain a given point?   Note that in some cases, this may
-- not correspond exactly with a server-side computation.   Consider @UTCTime@
-- for example, which has a resolution of a picosecond,  whereas postgresql's
-- @timestamptz@ types have a resolution of a microsecond.  Putting such
-- Haskell values into the database will result in them being rounded, which
-- can change the value of the containment predicate.

contains :: (HasCallStack, Ord a) => PGRange a -> (a -> Bool)
contains = containsBy compare

containsBy :: (HasCallStack) => (a -> a -> Ordering) -> PGRange a -> (a -> Bool)
containsBy cmp rng x =
    case rng of
      PGRange _lb         NegInfinity -> False
      PGRange lb          ub          -> checkLB lb x && checkUB ub x
  where
    checkLB lb x =
        case lb of
          NegInfinity -> True
          PosInfinity -> False
          Inclusive a -> cmp a x /= GT
          Exclusive a -> cmp a x == LT

    checkUB ub x =
        case ub of
          NegInfinity -> False
          PosInfinity -> True
          Inclusive z -> cmp x z /= GT
          Exclusive z -> cmp x z == LT

lowerBound :: (HasCallStack) => Parser (a -> RangeBound a)
lowerBound = (A.char '(' *> pure Exclusive) <|> (A.char '[' *> pure Inclusive)
{-# INLINE lowerBound #-}

upperBound :: (HasCallStack) => Parser (a -> RangeBound a)
upperBound = (A.char ')' *> pure Exclusive) <|> (A.char ']' *> pure Inclusive)
{-# INLINE upperBound #-}

-- | Generic range parser
pgrange :: (HasCallStack) => Parser (RangeBound B.ByteString, RangeBound B.ByteString)
pgrange = do
  lb <- lowerBound
  v1 <- (A.char ',' *> "") <|> (rangeElem (==',') <* A.char ',')
  v2 <- rangeElem $ \c -> c == ')' || c == ']'
  ub <- upperBound
  A.endOfInput
  let low = if B.null v1 then NegInfinity else lb v1
      up  = if B.null v2 then PosInfinity else ub v2
  return (low, up)

rangeElem :: (HasCallStack) => (Char -> Bool) -> Parser B.ByteString
rangeElem end = (A.char '"' *> doubleQuoted)
            <|> A.takeTill end
{-# INLINE rangeElem #-}

-- | Simple double quoted value parser
doubleQuoted :: (HasCallStack) => Parser B.ByteString
doubleQuoted = toByteString <$> go mempty
  where
    go acc = do
      h <- byteString <$> A.takeTill (\c -> c == '\\' || c == '"')
      let rest = do
           start <- A.anyChar
           case start of
             '\\' -> do
               c <- A.anyChar
               go (acc <> h <> char8 c)
             '"' -> (A.char '"' *> go (acc <> h <> char8 '"'))
                    <|> pure (acc <> h)
             _ -> error "impossible in doubleQuoted"
      rest

rangeToBuilder :: (HasCallStack, Ord a) => (a -> Builder) -> PGRange a -> Builder
rangeToBuilder = rangeToBuilderBy compare

-- | Generic range to builder for plain values
rangeToBuilderBy :: (HasCallStack) => (a -> a -> Ordering) -> (a -> Builder) -> PGRange a -> Builder
rangeToBuilderBy cmp f x =
    if isEmptyBy cmp x
    then byteString "'empty'"
    else let (PGRange a b) = x
          in buildLB a <> buildUB b
  where
    buildLB NegInfinity   = byteString "'[,"
    buildLB (Inclusive v) = byteString "'[\"" <> f v <> byteString "\","
    buildLB (Exclusive v) = byteString "'(\"" <> f v <> byteString "\","
    buildLB PosInfinity   = error "impossible in rangeToBuilder"

    buildUB NegInfinity   = error "impossible in rangeToBuilder"
    buildUB (Inclusive v) = char8 '"' <> f v <> byteString "\"]'"
    buildUB (Exclusive v) = char8 '"' <> f v <> byteString "\")'"
    buildUB PosInfinity   = byteString "]'"
{-# INLINE rangeToBuilder #-}


instance (FromField a, Typeable a) => FromField (PGRange a) where
  fromField f mdat = do
    info <- typeInfo f
    case info of
      Range{} ->
        let f' = f { typeOid = typoid (rngsubtype info) }
        in case mdat of
          Nothing -> returnError UnexpectedNull f ""
          Just "empty" -> pure $ empty
          Just bs ->
            let parseIt NegInfinity   = pure NegInfinity
                parseIt (Inclusive v) = Inclusive <$> fromField f' (Just v)
                parseIt (Exclusive v) = Exclusive <$> fromField f' (Just v)
                parseIt PosInfinity   = pure PosInfinity
            in case parseOnly pgrange bs of
                Left e -> returnError ConversionFailed f e
                Right (lb,ub) -> PGRange <$> parseIt lb <*> parseIt ub
      _ -> returnError Incompatible f ""


instance ToField (PGRange Int8) where
    toField = Plain . rangeToBuilder int8Dec
    {-# INLINE toField #-}

instance ToField (PGRange Int16) where
    toField = Plain . rangeToBuilder int16Dec
    {-# INLINE toField #-}

instance ToField (PGRange Int32) where
    toField = Plain . rangeToBuilder int32Dec
    {-# INLINE toField #-}

instance ToField (PGRange Int) where
    toField = Plain . rangeToBuilder intDec
    {-# INLINE toField #-}

instance ToField (PGRange Int64) where
    toField = Plain . rangeToBuilder int64Dec
    {-# INLINE toField #-}

instance ToField (PGRange Integer) where
    toField = Plain . rangeToBuilder integerDec
    {-# INLINE toField #-}

instance ToField (PGRange Word8) where
    toField = Plain . rangeToBuilder word8Dec
    {-# INLINE toField #-}

instance ToField (PGRange Word16) where
    toField = Plain . rangeToBuilder word16Dec
    {-# INLINE toField #-}

instance ToField (PGRange Word32) where
    toField = Plain . rangeToBuilder word32Dec
    {-# INLINE toField #-}

instance ToField (PGRange Word) where
    toField = Plain . rangeToBuilder wordDec
    {-# INLINE toField #-}

instance ToField (PGRange Word64) where
    toField = Plain . rangeToBuilder word64Dec
    {-# INLINE toField #-}

instance ToField (PGRange Float) where
    toField = Plain . rangeToBuilder floatDec
    {-# INLINE toField #-}

instance ToField (PGRange Double) where
    toField = Plain . rangeToBuilder doubleDec
    {-# INLINE toField #-}

instance ToField (PGRange Scientific) where
    toField = Plain . rangeToBuilder f
      where
        f = lazyByteString . LT.encodeUtf8 . LT.toLazyText . scientificBuilder
    {-# INLINE toField #-}

instance ToField (PGRange UTCTime) where
    toField = Plain . rangeToBuilder utcTimeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange ZonedTime) where
    toField = Plain . rangeToBuilderBy cmpZonedTime zonedTimeToBuilder
    {-# INLINE toField #-}

cmpZonedTime :: (HasCallStack) => ZonedTime -> ZonedTime -> Ordering
cmpZonedTime = compare `on` zonedTimeToUTC   -- FIXME:  optimize

instance ToField (PGRange LocalTime) where
    toField = Plain . rangeToBuilder localTimeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Day) where
    toField = Plain . rangeToBuilder dayToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange TimeOfDay) where
    toField = Plain . rangeToBuilder timeOfDayToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange UTCTimestamp) where
    toField = Plain . rangeToBuilder utcTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange ZonedTimestamp) where
    toField = Plain . rangeToBuilderBy cmpZonedTimestamp zonedTimestampToBuilder
    {-# INLINE toField #-}

cmpZonedTimestamp :: (HasCallStack) => ZonedTimestamp -> ZonedTimestamp -> Ordering
cmpZonedTimestamp = compare `on` (zonedTimeToUTC <$>)

instance ToField (PGRange LocalTimestamp) where
    toField = Plain . rangeToBuilder localTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Date) where
    toField = Plain . rangeToBuilder dateToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange NominalDiffTime) where
    toField = Plain . rangeToBuilder nominalDiffTimeToBuilder
    {-# INLINE toField #-}
