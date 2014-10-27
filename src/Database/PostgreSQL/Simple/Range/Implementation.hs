{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.PostgreSQL.Simple.Range.Implementation
       where

import           Blaze.ByteString.Builder             (Builder, fromByteString,
                                                       fromLazyByteString,
                                                       toByteString)
import           Blaze.ByteString.Builder.Char8       (fromChar)
import           Blaze.Text                           (double, float, integral)
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8     (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString.Char8     as A
import qualified Data.ByteString                      as B
import           Data.Int                             (Int16, Int32, Int64,
                                                       Int8)
import           Data.Maybe                           (fromMaybe, isNothing)
import           Data.Monoid                          (mempty)
import           Data.Scientific                      (Scientific)
import qualified Data.Text.Lazy.Builder               as LT
import qualified Data.Text.Lazy.Encoding              as LT
import           Data.Time                            (Day, LocalTime,
                                                       NominalDiffTime,
                                                       TimeOfDay, UTCTime,
                                                       ZonedTime)
import           Data.Typeable                        (Typeable)
import           Data.Word                            (Word, Word16, Word32,
                                                       Word64, Word8)

import           Database.PostgreSQL.Simple.Compat    (scientificBuilder, (<>))
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.ToField


-- | Represents boundary of a range
data RangeBound a = Inclusive a
                  | Exclusive a
     deriving (Show, Typeable, Eq, Ord, Functor)

-- | Generic range type
data PGRange a = EmptyRange
             | PGRange (Maybe (RangeBound a)) (Maybe (RangeBound a))
     deriving (Show, Typeable, Eq, Ord, Functor)


boundValue :: RangeBound t -> t
boundValue (Inclusive a) = a
boundValue (Exclusive a) = a

-- | check if range be represented as simple range
isSimpleRange :: PGRange t -> Bool
isSimpleRange (PGRange Nothing Nothing) = True
isSimpleRange (PGRange Nothing (Just (Exclusive _))) = True
isSimpleRange (PGRange (Just (Inclusive _)) Nothing) = True
isSimpleRange (PGRange (Just (Inclusive _)) (Just (Exclusive _))) = True
isSimpleRange _ = False

-- | Is range empty
isEmpty :: Eq a => PGRange a -> Bool
isEmpty EmptyRange = True
isEmpty (PGRange (Just (Inclusive _)) (Just (Inclusive _))) = False -- at least one elem
isEmpty (PGRange (Just l) (Just r)) = boundValue l == boundValue r
isEmpty _          = False

-- | Is range unbounded on both ends
isInfiniteRange :: PGRange t -> Bool
isInfiniteRange (PGRange Nothing Nothing) = True
isInfiniteRange _ = False

-- | @lower_inf@ in postgres
isLowerInfinite :: PGRange t -> Bool
isLowerInfinite (PGRange Nothing _) = True
isLowerInfinite _ = False

-- | @upper_inf@ in postgres
isUpperInfinite :: PGRange t -> Bool
isUpperInfinite (PGRange _ Nothing) = True
isUpperInfinite _ = False

-- | @lower_inc@ in postgres
isLowerInclusive :: PGRange t -> Bool
isLowerInclusive (PGRange (Just (Inclusive _)) _ ) = True
isLowerInclusive _ = False

-- | @upper_inc@ in postgres
isUpperInclusive :: PGRange t -> Bool
isUpperInclusive (PGRange _ (Just (Inclusive _))) = True
isUpperInclusive _ = False

-- | @lower@ in postgres
lowerBound :: PGRange t -> Maybe (RangeBound t)
lowerBound (PGRange l _ ) = l
lowerBound _ = Nothing

-- | @upper@ in postgres
upperBound :: PGRange t -> Maybe (RangeBound t)
upperBound (PGRange _ u ) = u
upperBound _ = Nothing

-- | Value is to the right of the boundary
satisfiesLeftBound :: Ord a => a -> RangeBound a -> Bool
satisfiesLeftBound v (Inclusive b) = v >= b
satisfiesLeftBound v (Exclusive b) = v > b

-- | Value is to the left of the boundary
satisfiesRightBound :: Ord a => a -> RangeBound a -> Bool
satisfiesRightBound v (Inclusive b) = v <= b
satisfiesRightBound v (Exclusive b) = v < b

-- | The first bound is not to the left of second bound
overlapsLeftBound :: Ord a => RangeBound a -> RangeBound a -> Bool
overlapsLeftBound (Inclusive v) (Inclusive b) = v >= b
overlapsLeftBound v b = boundValue v > boundValue b

-- | first boundary is further left from second boundary
extendsToLeftOfBound :: Ord a => RangeBound a -> RangeBound a -> Bool
extendsToLeftOfBound (Inclusive a) (Exclusive b) = a <= b
extendsToLeftOfBound a b = boundValue a < boundValue b

-- | first boundary is further right from second boundary
extendsToRightOfBound :: Ord a => RangeBound a -> RangeBound a -> Bool
extendsToRightOfBound (Inclusive a) (Exclusive b) = a >= b
extendsToRightOfBound a b = boundValue a > boundValue b

-- | The first bound is not to the right of second bound
overlapsRightBound :: Ord a => RangeBound a -> RangeBound a -> Bool
overlapsRightBound (Inclusive v) (Inclusive b) = v <= b
overlapsRightBound v b = boundValue v < boundValue b

-- | (@\@>@) for elements
containsElem :: Ord a => PGRange a -> a -> Bool
containsElem EmptyRange _ = False
containsElem (PGRange Nothing Nothing) _= True
containsElem (PGRange (Just l) Nothing) e = satisfiesLeftBound e l
containsElem (PGRange Nothing (Just r)) e = satisfiesRightBound e r
containsElem (PGRange (Just l) (Just r)) e =
    satisfiesLeftBound e l && satisfiesRightBound e r

-- | (@\@>@) for ranges
containsRange :: Ord a => PGRange a -> PGRange a -> Bool
containsRange a b = withinLeftBoundOf b a && withinRightBoundOf b a

-- | @&<@ does not extend to the right of
withinLeftBoundOf :: Ord a => PGRange a -> PGRange a -> Bool
withinLeftBoundOf EmptyRange _ = False
withinLeftBoundOf _ EmptyRange = False
withinLeftBoundOf (PGRange Nothing  _) (PGRange b _) = isNothing b
withinLeftBoundOf (PGRange (Just v) _) (PGRange b _) =
   fromMaybe True $ not . extendsToLeftOfBound v <$> b

-- | @&>@ does not extend to the left of
withinRightBoundOf :: Ord a => PGRange a -> PGRange a -> Bool
withinRightBoundOf EmptyRange _ = False
withinRightBoundOf _ EmptyRange = False
withinRightBoundOf (PGRange _  Nothing) (PGRange _ b) = isNothing b
withinRightBoundOf (PGRange _ (Just v)) (PGRange _ b) =
   fromMaybe True $ not . extendsToRightOfBound v <$> b

-- | @&&@ - do ranges have common points
overlaps :: Ord a => PGRange a -> PGRange a -> Bool
overlaps EmptyRange _ = False
overlaps _ EmptyRange = False
overlaps (PGRange ll lr) (PGRange rl rr) = left && right
    where
      left = case ll of
        Nothing -> True
        Just v -> fromMaybe True $ overlapsRightBound v <$> rr
      right = case lr of
        Nothing -> True
        Just v -> fromMaybe True $ overlapsLeftBound v <$> rl

------ Parsing ------

parseLowerBound :: Parser (a -> RangeBound a)
parseLowerBound = (A.char '(' *> pure Exclusive) <|> (A.char '[' *> pure Inclusive)
{-# INLINE parseLowerBound #-}

parseUpperBound :: Parser (a -> RangeBound a)
parseUpperBound = (A.char ')' *> pure Exclusive) <|> (A.char ']' *> pure Inclusive)
{-# INLINE parseUpperBound #-}

-- | Generic range parser
pgrange :: Parser (Maybe (RangeBound B.ByteString), Maybe (RangeBound B.ByteString))
pgrange = do
  lb <- parseLowerBound
  v1 <- (A.char ',' *> "") <|> (rangeElem (==',') <* A.char ',')
  v2 <- rangeElem $ \c -> c == ')' || c == ']'
  ub <- parseUpperBound
  A.endOfInput
  let low = if B.null v1 then Nothing else Just $ lb v1
      up  = if B.null v2 then Nothing else Just $ ub v2
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
rangeToBuilder _ EmptyRange = fromByteString "'empty'"
rangeToBuilder _ (PGRange Nothing Nothing) = fromByteString "'[,)'"
rangeToBuilder f (PGRange a b) = buildLB a <> buildUB b
  where
    buildLB Nothing = fromByteString "'[,"
    buildLB (Just bound) = case bound of
       Inclusive v -> fromByteString "'[\"" <> f v <> fromByteString "\","
       Exclusive v -> fromByteString "'(\"" <> f v <> fromByteString "\","

    buildUB Nothing = fromByteString "]'"
    buildUB (Just bound) = case bound of
        Inclusive v -> fromChar '"' <> f v <> fromByteString "\"]'"
        Exclusive v -> fromChar '"' <> f v <> fromByteString "\")'"
{-# INLINE rangeToBuilder #-}

------ instances -----
instance (FromField a, Typeable a) => FromField (PGRange a) where
  fromField f mdat = do
    info <- typeInfo f
    case info of
      Range{} ->
        let f' = f { typeOid = typoid (rngsubtype info) }
        in case mdat of
          Nothing -> returnError UnexpectedNull f ""
          Just "empty" -> pure $ EmptyRange
          Just bs ->
            let parseIt Nothing     = pure Nothing
                parseIt (Just (Inclusive v)) = Just . Inclusive <$> fromField f' (Just v)
                parseIt (Just (Exclusive v)) = Just . Exclusive <$> fromField f' (Just v)
            in case parseOnly pgrange bs of
                Left e -> returnError ConversionFailed f e
                Right (lb,ub) -> PGRange <$> parseIt lb <*> parseIt ub
      _ -> returnError Incompatible f ""


-- | Generic range ToField function, useful if you want to define your own
--   range types. Remember not to put your boundary value in quotes and
--   escape double quotes.
rangeToField :: (a -> Action) -> PGRange a -> Action
rangeToField _ EmptyRange = Plain $ fromByteString "'empty'"
rangeToField _ (PGRange Nothing Nothing) = Plain $ fromByteString "'[,)'"
rangeToField f (PGRange a b) = Many $ buildLB a ++ buildUB b
  where
    buildLB Nothing     = [ Plain $ fromByteString "'(," ]
    buildLB (Just bound) = case bound of
        Inclusive v -> [ Plain $ fromByteString "'[\"", f v
                       , Plain $ fromByteString "\"," ]
        Exclusive v -> [ Plain $ fromByteString "'(\"", f v
                       , Plain $ fromByteString "\"," ]
    buildUB Nothing     = [ Plain $ fromByteString "]'"]
    buildUB (Just bound) = case bound of
        Inclusive v -> [ Plain $ fromChar '"', f v
                       , Plain $ fromByteString "\"]'"]
        Exclusive v -> [ Plain $ fromChar '"', f v
                       , Plain $ fromByteString "\")'"]

integralRangeToBuilder :: (Integral a, Show a) => PGRange a -> Builder
integralRangeToBuilder = rangeToBuilder integral
{-# INLINE integralRangeToBuilder #-}

instance ToField (PGRange Int8) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int16) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int32) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int64) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Integer) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word8) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word16) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word32) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word64) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Float) where
    toField = Plain . rangeToBuilder float
    {-# INLINE toField #-}

instance ToField (PGRange Double) where
    toField = Plain . rangeToBuilder double
    {-# INLINE toField #-}

instance ToField (PGRange Scientific) where
    toField = Plain . rangeToBuilder f
      where
        f = fromLazyByteString . LT.encodeUtf8 . LT.toLazyText . scientificBuilder
    {-# INLINE toField #-}

instance ToField (PGRange UTCTime) where
    toField = Plain . rangeToBuilder utcTimeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange ZonedTime) where
    toField = Plain . rangeToBuilder zonedTimeToBuilder
    {-# INLINE toField #-}

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
    toField = Plain . rangeToBuilder zonedTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange LocalTimestamp) where
    toField = Plain . rangeToBuilder localTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Date) where
    toField = Plain . rangeToBuilder dateToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange NominalDiffTime) where
    toField = Plain . rangeToBuilder nominalDiffTimeToBuilder
    {-# INLINE toField #-}
