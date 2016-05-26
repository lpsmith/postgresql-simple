-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.LargeObjects
-- Copyright   :  (c) 2011-2012 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
--
-- Support for PostgreSQL's Large Objects;  see
-- <https://www.postgresql.org/docs/9.5/static/largeobjects.html> for more
-- information.
--
-- Note that Large Object File Descriptors are only valid within a single
-- database transaction,  so if you are interested in using anything beyond
-- 'loCreat', 'loCreate', and 'loUnlink',  you will need to run the entire
-- sequence of functions in a transaction.   As 'loImport' and 'loExport'
-- are simply C functions that call 'loCreat', 'loOpen', 'loRead', and
-- 'loWrite',  and do not perform any transaction handling themselves,
-- they also need to be wrapped in an explicit transaction.
--
-----------------------------------------------------------------------------

module Database.PostgreSQL.Simple.LargeObjects
     ( loCreat
     , loCreate
     , loImport
     , loImportWithOid
     , loExport
     , loOpen
     , loWrite
     , loRead
     , loSeek
     , loTell
     , loTruncate
     , loClose
     , loUnlink
     , Oid(..)
     , LoFd
     , IOMode(..)
     , SeekMode(..)
     ) where

import           Control.Applicative ((<$>))
import           Control.Exception (throwIO)
import qualified Data.ByteString as B
import           Database.PostgreSQL.LibPQ (Oid(..),LoFd(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.Internal
import           System.IO (IOMode(..),SeekMode(..))

liftPQ :: B.ByteString -> Connection -> (PQ.Connection -> IO (Maybe a)) -> IO a
liftPQ str conn m = withConnection conn $ \c -> do
    res <- m c
    case res of
      Nothing -> do
          msg <- maybe str id <$> PQ.errorMessage c
          throwIO $ fatalError msg
      Just  x -> return x

loCreat :: Connection -> IO Oid
loCreat conn = liftPQ "loCreat" conn (\c -> PQ.loCreat c)

loCreate :: Connection -> Oid -> IO Oid
loCreate conn oid = liftPQ "loCreate" conn (\c -> PQ.loCreate c oid)

loImport :: Connection -> FilePath -> IO Oid
loImport conn path = liftPQ "loImport" conn (\c -> PQ.loImport c path)

loImportWithOid :: Connection -> FilePath -> Oid -> IO Oid
loImportWithOid conn path oid = liftPQ "loImportWithOid" conn (\c -> PQ.loImportWithOid c path oid)

loExport :: Connection -> Oid -> FilePath -> IO ()
loExport conn oid path = liftPQ "loExport" conn (\c -> PQ.loExport c oid path)

loOpen :: Connection -> Oid -> IOMode -> IO LoFd
loOpen conn oid mode  = liftPQ "loOpen" conn (\c -> PQ.loOpen c oid mode )

loWrite :: Connection -> LoFd -> B.ByteString -> IO Int
loWrite conn fd dat = liftPQ "loWrite" conn (\c -> PQ.loWrite c fd dat)

loRead :: Connection -> LoFd -> Int -> IO B.ByteString
loRead conn fd maxlen = liftPQ "loRead" conn (\c -> PQ.loRead c fd maxlen)

loSeek :: Connection -> LoFd -> SeekMode -> Int -> IO Int
loSeek conn fd seekmode offset = liftPQ "loSeek" conn (\c -> PQ.loSeek c fd seekmode offset)

loTell :: Connection -> LoFd -> IO Int
loTell conn fd = liftPQ "loTell" conn (\c -> PQ.loTell c fd)

loTruncate :: Connection -> LoFd -> Int -> IO ()
loTruncate conn fd len = liftPQ "loTruncate" conn (\c -> PQ.loTruncate c fd len)

loClose :: Connection -> LoFd -> IO ()
loClose conn fd = liftPQ "loClose" conn (\c -> PQ.loClose c fd)

loUnlink :: Connection -> Oid -> IO ()
loUnlink conn oid = liftPQ "loUnlink" conn (\c -> PQ.loUnlink c oid)
