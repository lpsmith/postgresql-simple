{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.TypeInfo.Macro
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- A Template Haskell macro for efficiently checking membership in
-- a set of type oids.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.TypeInfo.Macro
    ( mkCompats
    , inlineTypoid
    ) where

import Database.PostgreSQL.Simple.TypeInfo.Static
import Database.PostgreSQL.Simple.Types (Oid(..))
import Language.Haskell.TH
import GHC.Stack


-- | Returns an expression that has type @'Oid' -> 'Bool'@,  true if the
--   oid is equal to any one of the 'typoid's of the given 'TypeInfo's.
mkCompats :: (HasCallStack) => [TypeInfo] -> ExpQ
mkCompats tys = [| \(Oid x) -> $(caseE [| x |] (map alt tys ++ [catchAll])) |]
   where
     alt :: (HasCallStack) => TypeInfo -> MatchQ
     alt ty = match (inlineTypoidP ty) (normalB [| True |]) []

     catchAll :: (HasCallStack) => MatchQ
     catchAll = match wildP (normalB [| False |]) []

-- | Literally substitute the 'typoid' of a 'TypeInfo' expression.
--   Returns an expression of type 'Oid'.  Useful because GHC tends
--   not to fold constants.
inlineTypoid :: (HasCallStack) => TypeInfo -> ExpQ
inlineTypoid ty = [| Oid $(litE (getTypoid ty)) |]

inlineTypoidP :: (HasCallStack) => TypeInfo -> PatQ
inlineTypoidP ty = litP (getTypoid ty)

getTypoid :: (HasCallStack) => TypeInfo -> Lit
getTypoid ty = let (Oid x) = typoid ty in integerL (fromIntegral x)
