{-# LANGUAGE TemplateHaskell #-}
module Database.PostgreSQL.Simple.FromRow.Boilerplate where

import Prelude
import Control.Applicative
import Control.Monad
import Data.List
import Language.Haskell.TH
import {-# SOURCE #-} Database.PostgreSQL.Simple.FromRow
import {-# SOURCE #-} Database.PostgreSQL.Simple.FromField

generateTupleInstance :: Int -> Q [Dec]
generateTupleInstance arity =
  pure $ (:[]) $ InstanceD constraints instanceHead decs
  where
    compositeType = foldl AppT (TupleT arity) typeVars
    typeVars = do
      i <- take arity [1..]
      return $ VarT $ mkName $ '_' : show i
    constraints = do
      typeVar <- typeVars
      return $ ClassP ''FromField [typeVar]
    instanceHead = ConT ''FromRow `AppT` compositeType
    decs = [fromRowDec]
      where
        fromRowDec = FunD 'fromRow [Clause [] (NormalB body) []]
          where
            body = processQueue queue
              where
                con = ConE $ tupleDataName arity
                queue =
                  (con :) $
                  (VarE '(<$>) :) $
                  intersperse (VarE '(<*>)) $
                  replicate arity (VarE 'field)
                processQueue q = case q of
                  e : o : tail -> UInfixE e o (processQueue tail)
                  e : [] -> e
                  _ -> error $ "Unexpected queue size"
