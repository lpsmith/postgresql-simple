{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.PostgreSQL.Simple.Range
       ( RangeBound(..)
       , PGRange (..)
       -- * Predicates
       , isEmpty
       , isInfiniteRange
       , isLowerInfinite
       , isUpperInfinite
       , isLowerInclusive
       , isUpperInclusive
       , isSimpleRange
       -- @ Functions on ranges
       , lowerBound
       , upperBound
       , containsElem
       , containsRange
       , overlaps
       , withinLeftBoundOf
       , withinRightBoundOf
       )
       where

import           Database.PostgreSQL.Simple.Range.Implementation
