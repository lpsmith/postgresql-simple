{-# LANGUAGE DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
module Database.PostgreSQL.Simple.ToRow (
      ToRow(..)
    ) where

import Database.PostgreSQL.Simple.Types
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToField
import GHC.Generics

class ToRow a where
    toRow :: a -> [Action]
    default toRow :: (Generic a, GToRow (Rep a)) => a -> [Action]
    toRow = gtoRow . from

class GToRow f where
    gtoRow :: f p -> [Action]

instance ToField a => ToRow (Only a)
