module Database.PostgreSQL.Simple.ToField where

import Database.PostgreSQL.Simple.Types

class ToField a

instance ToField Oid
