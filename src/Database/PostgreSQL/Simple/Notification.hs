{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
     , getNotificationNonBlocking
     ) where

import           Control.Concurrent
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
errfd = "Database.PostgreSQL.Simple.Notification.getNotification: failed to fetch file descriptor"

convertNotice :: PQ.Notify -> Notification
convertNotice PQ.Notify{..}
    = Notification { notificationPid     = notifyBePid
                   , notificationChannel = notifyRelname
                   , notificationData    = notifyExtra   }

-- | Returns a single notification.   If no notifications are available,
--   'getNotification' blocks until one arrives.

getNotification :: Connection -> IO Notification
getNotification conn = loop False
  where
    loop doConsume = do
        res <- withConnection conn $ \c -> do
                         when doConsume (PQ.consumeInput c >> return ())
                         mmsg <- PQ.notifies c
                         case mmsg of
                           Nothing -> do
                                         mfd <- PQ.socket c
                                         case mfd of
                                           Nothing -> fail errfd
                                           Just fd -> return (Left fd)
                           Just msg -> return (Right msg)
        -- FIXME? what happens if the connection is closed/reset right here?
        case res of
#if defined(mingw32_HOST_OS)
          -- threadWaitRead doesn't work for sockets on Windows, so just poll
          -- for input every second (PQconsumeInput is non-blocking).
          --
          -- We could call select(), but FFI calls can't be interrupted with
          -- async exceptions, whereas threadDelay can.
          Left _fd -> threadDelay 1000000 >> loop True
#else
          Left fd -> threadWaitRead fd >> loop True
#endif
          Right msg -> return $! convertNotice msg

-- | Non-blocking variant of 'getNotification'.   Returns a single notification,
-- if available.   If no notifications are available,  returns 'Nothing'.

getNotificationNonBlocking :: Connection -> IO (Maybe Notification)
getNotificationNonBlocking conn =
    withConnection conn $ \c -> do
        mmsg <- PQ.notifies c
        case mmsg of
          Just msg -> return $! Just $! convertNotice msg
          Nothing -> do
              _ <- PQ.consumeInput c
              mmsg' <- PQ.notifies c
              case mmsg' of
                Just msg -> return $! Just $! convertNotice msg
                Nothing  -> return Nothing
