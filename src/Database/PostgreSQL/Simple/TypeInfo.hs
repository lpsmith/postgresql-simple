{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.TypeInfo
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.TypeInfo
     ( getTypeInfo
     , NamedOid(..)
     , TypeInfo(..)
     ) where

import qualified Data.IntMap as IntMap
import           Control.Concurrent.MVar
import           Control.Exception (throw)

import qualified Database.PostgreSQL.LibPQ as PQ
import {-# SOURCE #-} Database.PostgreSQL.Simple
import                Database.PostgreSQL.Simple.Internal
import                Database.PostgreSQL.Simple.Types
import                Database.PostgreSQL.Simple.BuiltinTypes

getTypeInfo :: Connection -> PQ.Oid -> IO TypeInfo
getTypeInfo conn@Connection{..} oid =
  case oid2typname oid of
    Just name -> return $! TypeInfo { typ = NamedOid oid name
                                    , typelem = Nothing
                                    }
    Nothing -> modifyMVar connectionObjects $ \oidmap -> do
      case IntMap.lookup (oid2int oid) oidmap of
        Just typeinfo -> return (oidmap, typeinfo)
        Nothing -> do
            names <- query conn "SELECT p.oid, p.typname, c.oid, c.typname\
                               \ FROM pg_type AS p LEFT OUTER JOIN pg_type AS c\
                               \ ON c.oid = p.typelem\
                               \ WHERE p.oid = ?"
                                (Only oid)
            typinf <- case names of
                        []  -> return $ throw (fatalError "invalid type oid")
                        [(pOid, pTypName, mbCOid, mbCTypName)] ->
                            return $! TypeInfo { typ     = NamedOid pOid pTypName
                                               , typelem = do
                                                   cOid     <- mbCOid
                                                   cTypName <- mbCTypName
                                                   return $ NamedOid cOid cTypName
                                               }
                        _   -> fail "typename query returned more than one result"
                                 -- oid is a primary key,  so the query should
                                 -- never return more than one result
            return (IntMap.insert (oid2int oid) typinf oidmap, typinf)
