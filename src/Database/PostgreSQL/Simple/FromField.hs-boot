module Database.PostgreSQL.Simple.FromField where

import Data.ByteString(ByteString)
import Database.PostgreSQL.Simple.Types

class FromField a

instance FromField Oid
instance FromField Char
instance FromField ByteString
instance FromField a => FromField (Maybe a)
