{-# LANGUAGE BangPatterns, OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.QueryResults
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'QueryResults' typeclass, for converting a row of results
-- returned by a SQL query into a more useful Haskell representation.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.QueryResults
    (
      QueryResults(..)
    , convertError
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (SomeException(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either()
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Result (ResultError(..), Result(..))
import Database.PostgreSQL.Simple.Types (Only(..))

-- | A collection type that can be converted from a list of strings.
--
-- Instances should use the 'convert' method of the 'Result' class
-- to perform conversion of each element of the collection.
--
-- This example instance demonstrates how to convert a two-column row
-- into a Haskell pair. Each field in the metadata is paired up with
-- each value from the row, and the two are passed to 'convert'.
--
-- @
-- instance ('Result' a, 'Result' b) => 'QueryResults' (a,b) where
--     'convertResults' [fa,fb] [va,vb] = do
--               !a <- 'convert' fa va
--               !b <- 'convert' fb vb
--               'return' (a,b)
--     'convertResults' fs vs  = 'convertError' fs vs 2
-- @
--
-- Notice that this instance evaluates each element to WHNF before
-- constructing the pair.  This property is important enough that its
-- a rule all 'QueryResult' instances should follow:
--
--   * Evaluate every 'Result' value to WHNF before constructing
--     the result
--
-- Doing so keeps resource usage under local control by preventing the
-- construction of potentially long-lived thunks that are forced
-- (or not) by the consumer.
--
-- This is important to postgresql-simple-0.0.4 because a wayward thunk
-- causes the entire LibPQ.'LibPQ.Result' to be retained. This could lead
-- to a memory leak, depending on how the thunk is consumed.
--
-- Note that instances can be defined outside of postgresql-simple,
-- which is often useful.   For example,  here is an attempt at an
-- instance for a user-defined pair:
--
-- @
-- data User = User { firstName :: String, lastName :: String }
--
-- instance 'QueryResults' User where
--     'convertResults' [fa,qfb] [va,vb] = User \<$\> a \<*\> b
--        where  !a =  'convert' fa va
--               !b =  'convert' fb vb
--     'convertResults' fs vs  = 'convertError' fs vs 2
-- @
--
-- In this example,  the bang patterns are not used correctly.  They force
-- the data constructors of the 'Either' type,  and are not forcing the
-- 'Result' values we need to force.  This gives the consumer of the
-- 'QueryResult' the ability to cause the memory leak,  which is an
-- undesirable state of affairs.

class QueryResults a where
    convertResults :: [Field] -> [Maybe ByteString] -> Either SomeException a
    -- ^ Convert values from a row into a Haskell collection.
    --
    -- This function will return a 'ResultError' if conversion of the
    -- collection fails.

instance (Result a) => QueryResults (Only a) where
    convertResults [fa] [va] = do
              !a <- convert fa va
              return (Only a)
    convertResults fs vs  = convertError fs vs 1

instance (Result a, Result b) => QueryResults (a,b) where
    convertResults [fa,fb] [va,vb] = do
              !a <- convert fa va
              !b <- convert fb vb
              return (a,b)
    convertResults fs vs  = convertError fs vs 2

instance (Result a, Result b, Result c) => QueryResults (a,b,c) where
    convertResults [fa,fb,fc] [va,vb,vc] = do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              return (a,b,c)
    convertResults fs vs  = convertError fs vs 3

instance (Result a, Result b, Result c, Result d) =>
    QueryResults (a,b,c,d) where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              return (a,b,c,d)
    convertResults fs vs  = convertError fs vs 4

instance (Result a, Result b, Result c, Result d, Result e) =>
    QueryResults (a,b,c,d,e) where
    convertResults [fa,fb,fc,fd,fe] [va,vb,vc,vd,ve] = do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              !e <- convert fe ve
              return (a,b,c,d,e)
    convertResults fs vs  = convertError fs vs 5

instance (Result a, Result b, Result c, Result d, Result e, Result f) =>
    QueryResults (a,b,c,d,e,f) where
    convertResults [fa,fb,fc,fd,fe,ff] [va,vb,vc,vd,ve,vf] = do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              !e <- convert fe ve
              !f <- convert ff vf
              return (a,b,c,d,e,f)
    convertResults fs vs  = convertError fs vs 6

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g) =>
    QueryResults (a,b,c,d,e,f,g) where
    convertResults [fa,fb,fc,fd,fe,ff,fg] [va,vb,vc,vd,ve,vf,vg] = do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              !e <- convert fe ve
              !f <- convert ff vf
              !g <- convert fg vg
              return (a,b,c,d,e,f,g)
    convertResults fs vs  = convertError fs vs 7

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h) =>
    QueryResults (a,b,c,d,e,f,g,h) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh] [va,vb,vc,vd,ve,vf,vg,vh] = do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              !e <- convert fe ve
              !f <- convert ff vf
              !g <- convert fg vg
              !h <- convert fh vh
              return (a,b,c,d,e,f,g,h)
    convertResults fs vs  = convertError fs vs 8

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i) =>
    QueryResults (a,b,c,d,e,f,g,h,i) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi] [va,vb,vc,vd,ve,vf,vg,vh,vi] =
           do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              !e <- convert fe ve
              !f <- convert ff vf
              !g <- convert fg vg
              !h <- convert fh vh
              !i <- convert fi vi
              return (a,b,c,d,e,f,g,h,i)
    convertResults fs vs  = convertError fs vs 9

instance (Result a, Result b, Result c, Result d, Result e, Result f,
          Result g, Result h, Result i, Result j) =>
    QueryResults (a,b,c,d,e,f,g,h,i,j) where
    convertResults [fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]
                   [va,vb,vc,vd,ve,vf,vg,vh,vi,vj] =
           do
              !a <- convert fa va
              !b <- convert fb vb
              !c <- convert fc vc
              !d <- convert fd vd
              !e <- convert fe ve
              !f <- convert ff vf
              !g <- convert fg vg
              !h <- convert fh vh
              !i <- convert fi vi
              !j <- convert fj vj
              return (a,b,c,d,e,f,g,h,i,j)
    convertResults fs vs  = convertError fs vs 10

(<$!>) :: Functor f => (a -> b) -> f a -> f b
f <$!> (!x) = f <$> x
infixl 4 <$!>

instance Result a => QueryResults [a] where
    convertResults fs vs = foldr convert' (pure []) (zip fs vs)
      where convert' (f,v) as = (:) <$!> convert f v <*> as
    {-# INLINE convertResults #-}

-- | Throw a 'ConversionFailed' exception, indicating a mismatch
-- between the number of columns in the 'Field' and row, and the
-- number in the collection to be converted to.
convertError :: [Field]
             -- ^ Descriptors of fields to be converted.
             -> [Maybe ByteString]
             -- ^ Contents of the row to be converted.
             -> Int
             -- ^ Number of columns expected for conversion.  For
             -- instance, if converting to a 3-tuple, the number to
             -- provide here would be 3.
             -> Either SomeException a
convertError fs vs n = Left . SomeException $ ConversionFailed
    (show (length fs) ++ " values: " ++ show (zip (map typename fs)
                                                  (map (fmap ellipsis) vs)))
    (show n ++ " slots in target type")
    "mismatch between number of columns to convert and number in target type"

ellipsis :: ByteString -> ByteString
ellipsis bs
    | B.length bs > 15 = B.take 10 bs `B.append` "[...]"
    | otherwise        = bs

