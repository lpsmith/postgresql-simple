{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

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

-- | Returns an expression that has type @'Oid' -> 'Bool'@,  true if the
--   oid is equal to any one of the 'typoid's of the given 'TypeInfo's.
mkCompats :: [TypeInfo] -> ExpQ
mkCompats tys = do
    x <- newName "x"
    lamE [conP 'Oid [varP x]] $ caseE (varE x) (map alt tys ++ [catchAll])
   where
     alt :: TypeInfo -> MatchQ
     alt ty = match (inlineTypoidP ty) (normalB [| True |]) []

     catchAll :: MatchQ
     catchAll = match wildP (normalB [| False |]) []

-- | Literally substitute the 'typoid' of a 'TypeInfo' expression.
--   Returns an expression of type 'Oid'.  Useful because GHC tends
--   not to fold constants.
inlineTypoid :: TypeInfo -> ExpQ
inlineTypoid ty = conE 'Oid `appE` litE (getTypoid ty)

inlineTypoidP :: TypeInfo -> PatQ
inlineTypoidP ty = litP (getTypoid ty)

getTypoid :: TypeInfo -> Lit
getTypoid ty = let (Oid x) = typoid ty in integerL (fromIntegral x)
