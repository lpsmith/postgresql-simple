{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.TypeInfo
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- This module provides convenient and efficient access to parts of the
-- @pg_type@ metatable.  At the moment, this requires PostgreSQL 8.4 if
-- you need to work with types that do not appear in
-- 'Database.PostgreSQL.Simple.TypeInfo.Static'.
--
-- The current scheme could be more efficient, especially for some use
-- cases.  In particular,  connection pools that use many user-added
-- types and connect to a set of servers with identical (or at least
-- compatible) @pg_type@ and associated tables could share a common
-- typeinfo cache,  thus saving memory and communication between the
-- client and server.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.TypeInfo
     ( getTypeInfo
     , TypeInfo(..)
     ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.IntMap as IntMap
import           Control.Concurrent.MVar
import           Control.Exception (throw)

import qualified Database.PostgreSQL.LibPQ as PQ
import {-# SOURCE #-} Database.PostgreSQL.Simple
import                Database.PostgreSQL.Simple.Internal
import                Database.PostgreSQL.Simple.Types
import                Database.PostgreSQL.Simple.TypeInfo.Types
import                Database.PostgreSQL.Simple.TypeInfo.Static

-- | Returns the metadata of the type with a particular oid.  To find
--   this data, 'getTypeInfo' first consults postgresql-simple's
--   built-in 'staticTypeInfo' table,  then checks  the connection's
--   typeinfo cache.   Finally,  the database's 'pg_type' table will
--   be queried only if necessary,  and the result will be stored
--   in the connections's cache.

getTypeInfo :: Connection -> PQ.Oid -> IO TypeInfo
getTypeInfo conn@Connection{..} oid =
  case staticTypeInfo oid of
    Just name -> return name
    Nothing -> modifyMVar connectionObjects $ getTypeInfo' conn oid

getTypeInfo' :: Connection -> PQ.Oid -> TypeInfoCache
             -> IO (TypeInfoCache, TypeInfo)
getTypeInfo' conn oid oidmap =
  case IntMap.lookup (oid2int oid) oidmap of
    Just typeinfo -> return (oidmap, typeinfo)
    Nothing -> do
      names  <- query conn "SELECT oid, typcategory, typdelim, typname, typelem\
                         \ FROM pg_type WHERE oid = ?"
                           (Only oid)
      (oidmap', typeInfo) <-
          case names of
            []  -> return $ throw (fatalError "invalid type oid")
            [(typoid, typcategory_, typdelim_, typname, typelem_)] -> do
               let !typcategory = B8.index typcategory_ 0
                   !typdelim    = B8.index typdelim_    0
               if typcategory == 'A'
                 then do
                   (oidmap', typelem) <- getTypeInfo' conn typelem_ oidmap
                   let !typeInfo = Array{..}
                   return (oidmap', typeInfo)
                 else do
                   let !typeInfo = Basic{..}
                   return (oidmap, typeInfo)
            _ -> fail "typename query returned more than one result"
                   -- oid is a primary key,  so the query should
                   -- never return more than one result
      let !oidmap'' = IntMap.insert (oid2int oid) typeInfo oidmap'
      return $! (oidmap'', typeInfo)
