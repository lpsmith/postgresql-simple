{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.PostgreSQL.Simple.Range.Simple
       where

import           Control.Applicative
import           Data.Maybe                                      (fromMaybe,
                                                                  isNothing)
import           Data.Typeable                                   (Typeable)

import           Database.PostgreSQL.Simple.FromField            (FromField (..), ResultError (..),
                                                                  returnError)
import           Database.PostgreSQL.Simple.Range.Implementation (PGRange (..), RangeBound (..))
import           Database.PostgreSQL.Simple.ToField              (ToField (..))
-- | Simple range type for not empty ranges alway inclusive on
-- lower-bound and exclusive on upper bound @[lower-bound,upper-bound)@
data SimpleRange a = SimpleRange (Maybe a) (Maybe a)
     deriving (Show, Typeable, Eq, Ord, Functor)

-- | convert 'SimpleRange' to 'PGRange'
simpleToRange :: SimpleRange a -> PGRange a
simpleToRange (SimpleRange a b) = PGRange (Inclusive <$> a) (Exclusive <$> b)
{-# INLINE simpleToRange #-}

-- | try to convert 'PGRange' to 'SimpleRange' if it can be represented as such
mayToSimpleRange :: PGRange a -> Maybe (SimpleRange a)
mayToSimpleRange (PGRange a b) = SimpleRange <$> low <*> up
  where
    low = case a of
      Nothing -> Just Nothing
      (Just (Inclusive v)) -> Just (Just v)
      _ -> Nothing
    up = case b of
      Nothing -> Just Nothing
      (Just (Exclusive v)) -> Just (Just v)
      _ -> Nothing
mayToSimpleRange _ = Nothing

-- | Convert non empty range to canonicalized discrete @[lower,upper)@ range
mayToDiscreteRange :: Enum a => PGRange a -> Maybe (SimpleRange a)
mayToDiscreteRange (PGRange a b) = Just $ SimpleRange (fmap toLow a) (fmap toUp b)
  where
    toLow (Exclusive v) = pred v
    toLow (Inclusive v) = v
    toUp (Exclusive v) = v
    toUp (Inclusive v) = succ v
mayToDiscreteRange _ = Nothing

-- | Build simple range from bounds, fixing order. Nothing for empty range
simpleRange :: Ord a => a -> a -> Maybe (SimpleRange a)
simpleRange a b = case compare a b of
   LT -> Just $ SimpleRange (Just a) (Just b)
   GT -> Just $ SimpleRange (Just b) (Just a)
   EQ -> Nothing

-- | Build simple range from bounds, fixing order. errors on empty range
simpleRangeErr :: Ord a => a -> a -> SimpleRange a
simpleRangeErr a b = case compare a b of
   LT -> SimpleRange (Just a) (Just b)
   GT -> SimpleRange (Just b) (Just a)
   EQ -> error "simpleRangeErr results in empty range"

-- | Is range unbounded on both ends
isInfiniteRange :: SimpleRange a -> Bool
isInfiniteRange (SimpleRange Nothing Nothing) = True
isInfiniteRange _ = False

-- | @lower_inf@ in postgres
isLowerInfinite :: SimpleRange a -> Bool
isLowerInfinite (SimpleRange Nothing _) = True
isLowerInfinite _ = False

-- | @upper_inf@ in postgres
isUpperInfinite :: SimpleRange a -> Bool
isUpperInfinite (SimpleRange _ Nothing) = True
isUpperInfinite _ = False

-- | @lower@ in postgres
lowerBound :: SimpleRange a -> Maybe a
lowerBound (SimpleRange l _ ) = l

-- | @upper@ in postgres
upperBound :: SimpleRange a -> Maybe a
upperBound (SimpleRange _ u ) = u


-- | (@\@>@)
containsElem :: Ord a => SimpleRange a -> a -> Bool
containsElem (SimpleRange Nothing  Nothing)  _ = True
containsElem (SimpleRange (Just l) Nothing)  e = e >= l
containsElem (SimpleRange Nothing  (Just r)) e = e < r
containsElem (SimpleRange (Just l) (Just r)) e = e >= l && e < r

-- | (@\@>@) for ranges
containsRange :: Ord a => SimpleRange a -> SimpleRange a -> Bool
containsRange a b = withinLeftBoundOf b a && withinRightBoundOf b a

-- | @&&@ - do ranges have points in common
overlaps :: Ord a => SimpleRange a -> SimpleRange a -> Bool
overlaps (SimpleRange ll lr) (SimpleRange rl rr) = left && right
  where
    left = case ll of
      Nothing -> True
      Just v -> fromMaybe True $ (v <=) <$> rr
    right = case lr of
      Nothing -> True
      Just v -> fromMaybe True $ (v >=) <$> rl

-- | @<<@ is strictly left of
isLeftOf :: Ord a => SimpleRange a -> SimpleRange a -> Bool
isLeftOf _ (SimpleRange Nothing _) = False
isLeftOf (SimpleRange _ Nothing) _ = False
isLeftOf (SimpleRange _ (Just l)) (SimpleRange (Just r) _) = l <= r

-- | @<<@ is strictly right of
isRightOf :: Ord a => SimpleRange a -> SimpleRange a -> Bool
isRightOf _ (SimpleRange _ Nothing) = False
isRightOf (SimpleRange Nothing _) _ = False
isRightOf (SimpleRange (Just l) _) (SimpleRange _ (Just r)) = l > r

-- | @&<@ does not extend to the right of
withinRightBoundOf :: Ord a => SimpleRange a -> SimpleRange a -> Bool
withinRightBoundOf (SimpleRange _ Nothing) (SimpleRange _ r) = isNothing r
withinRightBoundOf (SimpleRange _ (Just v)) (SimpleRange _ r) = fromMaybe True $ (v <= ) <$> r

-- | @&>@ does not extend to the left of
withinLeftBoundOf :: Ord a => SimpleRange a -> SimpleRange a -> Bool
withinLeftBoundOf (SimpleRange Nothing _) (SimpleRange r _) = isNothing r
withinLeftBoundOf (SimpleRange (Just v) _) (SimpleRange r _) = fromMaybe True $ (v >= ) <$> r

-- | Right bound equals left bound of other range
adjacentToLeftOf :: Eq a => SimpleRange a -> SimpleRange a -> Bool
adjacentToLeftOf (SimpleRange _ Nothing) _ = False
adjacentToLeftOf _ (SimpleRange Nothing _) = False
adjacentToLeftOf (SimpleRange _ (Just l)) (SimpleRange (Just r) _) = l == r

-- | Left bound equals right bound of other range
adjacentToRightOf :: Eq a => SimpleRange a -> SimpleRange a -> Bool
adjacentToRightOf (SimpleRange Nothing _) _ = False
adjacentToRightOf _ (SimpleRange _ Nothing) = False
adjacentToRightOf (SimpleRange (Just l) _) (SimpleRange _ (Just r)) = l == r

-- | Either left or right bound is adjacent to other range
adjacentTo :: Eq a => SimpleRange a -> SimpleRange a -> Bool
adjacentTo l r = adjacentToLeftOf l r && adjacentToRightOf l r


instance (ToField (PGRange a)) => ToField (SimpleRange a) where
  toField = toField . simpleToRange
  {-# INLINE toField #-}

instance (Typeable a, FromField (PGRange a)) => FromField (SimpleRange a) where
  fromField f mb = do
     pgr <- fromField f mb
     case pgr of
       EmptyRange -> returnError ConversionFailed f "Empty range encountered"
       PGRange l r -> do
         leftB <- case l of
           Nothing -> return Nothing
           (Just (Inclusive v)) -> return $ Just v
           _ -> returnError ConversionFailed f "Exclusive left bound encountered"
         rightB <- case r of
           Nothing -> return Nothing
           (Just (Exclusive v)) -> return $ Just v
           _ -> returnError ConversionFailed f "Inclusive right bound encountered"
         return $ SimpleRange leftB rightB
  {-# INLINE fromField #-}
