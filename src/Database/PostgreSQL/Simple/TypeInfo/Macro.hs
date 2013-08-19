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
    ( mkCompat
    , mkCompats 
    ) where

import Database.PostgreSQL.Simple.TypeInfo.Static
import Database.PostgreSQL.Simple.Types (Oid(..))
import Language.Haskell.TH

mkCompat :: TypeInfo -> ExpQ
mkCompat ty = [| \(Oid x) -> x == $(litE (getTypoid ty)) |]

mkCompats :: [TypeInfo] -> ExpQ
mkCompats tys = [| \(Oid x) -> $(caseE [| x |] (map alt tys ++ [catchAll])) |]
   where
     alt :: TypeInfo -> MatchQ
     alt ty = match (litP (getTypoid ty)) (normalB [| True |]) []
     
     catchAll :: MatchQ
     catchAll = match wildP (normalB [| False |]) []

getTypoid :: TypeInfo -> Lit
getTypoid ty = let (Oid x) = typoid ty in integerL (fromIntegral x)
