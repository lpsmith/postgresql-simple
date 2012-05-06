{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.Notification
-- Copyright   :  (c) 2011-2012 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
--
-- Support for receiving asynchronous notifications via PostgreSQL's
-- Listen/Notify mechanism.  See
-- <http://www.postgresql.org/docs/9.1/static/sql-notify.html> for more
-- information.
--
-----------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Notification
     ( Notification(..)
     , getNotification
     ) where

import           Control.Concurrent ( threadWaitRead )
import           Control.Monad ( when )
import qualified Data.ByteString as B
import           Database.PostgreSQL.Simple.Internal
import qualified Database.PostgreSQL.LibPQ as PQ
import           System.Posix.Types ( CPid )

data Notification = Notification
   { notificationPid     :: !CPid
   , notificationChannel :: !B.ByteString
   , notificationData    :: !B.ByteString
   }

errfd :: String
errfd   = "Database.PostgreSQL.Simple.Notification.getNotification: \
          \failed to fetch file descriptor"


-- | Returns a single notification.   If no notifications are available,
--   'getNotification' blocks until one arrives.

getNotification :: Connection -> IO Notification
getNotification = loop False
  where
    loop doConsume conn = do
        res <- withConnection conn $ \c -> do
                         when doConsume (PQ.consumeInput c >> return ())
                         mmsg <- PQ.notifies c
                         case mmsg of
                           Nothing -> do
                                         mfd <- PQ.socket c
                                         case mfd of
                                           Nothing -> fail errfd
                                           Just fd -> return (Left fd)
                           Just x -> return (Right x)
        case res of
          Left fd -> threadWaitRead fd >> loop True conn
          Right PQ.Notify{..} -> do
              return Notification { notificationPid     = notifyBePid
                                  , notificationChannel = notifyRelname
                                  , notificationData    = notifyExtra   }
