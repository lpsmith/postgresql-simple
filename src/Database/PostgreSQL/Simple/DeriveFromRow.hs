{-# LANGUAGE TemplateHaskell #-}
module Database.PostgreSQL.Simple.DeriveFromRow (deriveFromRow) where

import Language.Haskell.TH
import Control.Applicative ((<$>), (<*>))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

consInfo (TyConI (DataD _ typeName _ [RecC consName types] _)) = return (typeName, consName, length types)
consInfo (TyConI (DataD _ typeName _ [NormalC consName types] _)) = return (typeName, consName, length types)
consInfo _ = fail "Expected data type with single constructor"

-- | `deriveFromRow` automatically derives a `FromRow` instance from a
-- data declaration. The data declaration must have a single type
-- constructor.
--
-- Example:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > data Foo = Foo String Integer
-- >
-- > deriveFromRow ''Foo
-- >
-- > getFoo :: Connection -> IO [Foo]
-- > getFoo con = query_ con "SELECT 'hello', 1"
--
-- The line
--
-- > deriveFromRow ''Foo
--
-- is equivalent to:
--
-- > instance FromRow Foo where
-- >   fromRow = Foo <$> field <*> field

deriveFromRow :: Name -> Q [Dec]
deriveFromRow t = do
  (typeName, consName, numArgs) <- (reify t >>= consInfo)
  let impl 0 = return $ ConE consName
      impl 1 = [| $(impl 0) <$> field |]
      impl n = [| $(impl (n - 1)) <*> field |]
  [d| instance FromRow $(return $ ConT typeName) where fromRow = $(impl numArgs) |]
