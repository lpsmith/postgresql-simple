module Database.PostgreSQL.Simple.FromRow where

import {-# SOURCE #-} Database.PostgreSQL.Simple.FromField
import                Database.PostgreSQL.Simple.Types

class FromRow a

instance (FromField a) => FromRow (Only a)
instance (FromField a, FromField b)
      => FromRow (a,b)
instance (FromField a, FromField b, FromField c, FromField d)
      => FromRow (a,b,c,d)
instance (FromField a, FromField b, FromField c, FromField d, FromField e)
      => FromRow (a,b,c,d,e)
instance (FromField a, FromField b, FromField c, FromField d, FromField e
         ,FromField f)
      => FromRow (a,b,c,d,e,f)
