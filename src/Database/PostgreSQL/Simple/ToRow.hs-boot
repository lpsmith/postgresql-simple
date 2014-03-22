module Database.PostgreSQL.Simple.ToRow where

import Database.PostgreSQL.Simple.Types
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToField

class ToRow a where
    toRow :: a -> [Action]

instance ToField a => ToRow (Only a)
