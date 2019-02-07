{-# LANGUAGE DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.ToRow
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- The 'ToRow' typeclass, for rendering a collection of
-- parameters to a SQL query.
--
-- Predefined instances are provided for tuples containing up to ten
-- elements.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.ToRow
    (
      ToRow(..)
    ) where

import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import Database.PostgreSQL.Simple.Types (Only(..), (:.)(..))
import GHC.Generics

-- | A collection type that can be turned into a list of rendering
-- 'Action's.
--
-- Instances should use the 'toField' method of the 'ToField' class
-- to perform conversion of each element of the collection.
--
-- You can derive 'ToRow' for your data type using GHC generics, like this:
--
-- @
-- \{-# LANGUAGE DeriveAnyClass \#-}
-- \{-# LANGUAGE DeriveGeneric  \#-}
--
-- import "GHC.Generics" ('GHC.Generics.Generic')
-- import "Database.PostgreSQL.Simple" ('ToRow')
--
-- data User = User { name :: String, fileQuota :: Int }
--   deriving ('GHC.Generics.Generic', 'ToRow')
-- @
--
-- Note that this only works for product types (e.g. records) and does not
-- support sum types or recursive types.
class ToRow a where
    toRow :: a -> [Action]
    default toRow :: (Generic a, GToRow (Rep a)) => a -> [Action]
    toRow = gtoRow . from
    -- ^ ToField a collection of values.

instance ToRow () where
    toRow _ = []

instance (ToField a) => ToRow (Only a) where
    toRow (Only v) = [toField v]

instance (ToField a, ToField b) => ToRow (a,b) where
    toRow (a,b) = [toField a, toField b]

instance (ToField a, ToField b, ToField c) => ToRow (a,b,c) where
    toRow (a,b,c) = [toField a, toField b, toField c]

instance (ToField a, ToField b, ToField c, ToField d) => ToRow (a,b,c,d) where
    toRow (a,b,c,d) = [toField a, toField b, toField c, toField d]

instance (ToField a, ToField b, ToField c, ToField d, ToField e)
    => ToRow (a,b,c,d,e) where
    toRow (a,b,c,d,e) =
        [toField a, toField b, toField c, toField d, toField e]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f)
    => ToRow (a,b,c,d,e,f) where
    toRow (a,b,c,d,e,f) =
        [toField a, toField b, toField c, toField d, toField e, toField f]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g)
    => ToRow (a,b,c,d,e,f,g) where
    toRow (a,b,c,d,e,f,g) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h)
    => ToRow (a,b,c,d,e,f,g,h) where
    toRow (a,b,c,d,e,f,g,h) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i)
    => ToRow (a,b,c,d,e,f,g,h,i) where
    toRow (a,b,c,d,e,f,g,h,i) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j)
    => ToRow (a,b,c,d,e,f,g,h,i,j) where
    toRow (a,b,c,d,e,f,g,h,i,j) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s]

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o, ToField p, ToField q, ToField r,
          ToField s, ToField t)
    => ToRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) =
        [toField a, toField b, toField c, toField d, toField e, toField f,
         toField g, toField h, toField i, toField j, toField k, toField l,
         toField m, toField n, toField o, toField p, toField q, toField r,
         toField s, toField t]

instance (ToField a) => ToRow [a] where
    toRow = map toField

instance (ToRow a, ToRow b) => ToRow (a :. b) where
    toRow (a :. b) = toRow a ++ toRow b


-- Type class for default implementation of ToRow using generics
class GToRow f where
    gtoRow :: f p -> [Action]

instance GToRow f => GToRow (M1 c i f) where
    gtoRow (M1 x) = gtoRow x

instance (GToRow f, GToRow g) => GToRow (f :*: g) where
    gtoRow (f :*: g) = gtoRow f ++ gtoRow g

instance (ToField a) => GToRow (K1 R a) where
    gtoRow (K1 a) = [toField a]

instance GToRow U1 where
    gtoRow _ = []
