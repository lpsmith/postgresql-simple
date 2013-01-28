module Database.PostgreSQL.Simple.ToRow where

import Database.PostgreSQL.Simple.Types
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToField

class ToRow a

instance ToField a => ToRow (Only a)
