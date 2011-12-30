{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.LargeObjects
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
--
-----------------------------------------------------------------------------

module Database.PostgreSQL.Simple.LargeObjects
     ( loImport
     , loExport
     , Oid(..)
     ) where

import           Control.Applicative ((<$>))
import           Control.Exception (throwIO)
import           Database.PostgreSQL.LibPQ (Oid(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.Internal
import           Foreign.C.Types(CInt)

loImport :: Connection -> FilePath -> IO Oid
loImport conn path = withConnection conn $ \c -> do
    res <- PQ.loImport c path
    case res of
      Nothing -> do
          msg <- maybe "loImport error" id <$> PQ.errorMessage c
          throwIO $ SqlError { sqlNativeError = -1   -- FIXME?
                             , sqlErrorMsg    = msg
                             , sqlState       = ""  }
      Just  x -> return x


loExport :: Connection -> Oid -> FilePath -> IO ()
loExport conn oid path = withConnection conn $ \c -> do
    res <- PQ.loExport c oid path
    case res of
      Nothing -> do
          msg <- maybe "loExport error" id <$> PQ.errorMessage c
          throwIO $ SqlError { sqlNativeError = -1   -- FIXME?
                             , sqlErrorMsg    = msg
                             , sqlState       = ""  }
      Just  x -> return x
