module Database.PostgreSQL.Simple.FromRow where

import {-# SOURCE #-} Database.PostgreSQL.Simple.FromField
import                Database.PostgreSQL.Simple.Types
import                Database.PostgreSQL.Simple.Internal

class FromRow a where
    fromRow :: RowParser a

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

field :: FromField a => RowParser a
