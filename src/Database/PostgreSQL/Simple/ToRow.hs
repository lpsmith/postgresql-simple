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
