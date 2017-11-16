{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.Notification
-- Copyright   :  (c) 2011-2015 Leon P Smith
--                (c) 2012 Joey Adams
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
--
-- Support for receiving asynchronous notifications via PostgreSQL's
-- Listen/Notify mechanism.  See
-- <https://www.postgresql.org/docs/9.5/static/sql-notify.html> for more
-- information.
--
-- Note that on Windows,  @getNotification@ currently uses a polling loop
-- of 1 second to check for more notifications,  due to some inadequacies
-- in GHC's IO implementation and interface on that platform.   See GHC
-- issue #7353 for more information.  While this workaround is less than
-- ideal,  notifications are still better than polling the database directly.
-- Notifications do not create any extra work for the backend,  and are
-- likely cheaper on the client side as well.
--
-- <https://hackage.haskell.org/trac/ghc/ticket/7353>
--
-----------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Notification
     ( Notification(..)
     , getNotification
     , getNotificationNonBlocking
     , getBackendPID
     ) where

import           Control.Monad ( join, void )
import           Control.Exception ( throwIO, catch )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Database.PostgreSQL.Simple.Internal
import qualified Database.PostgreSQL.LibPQ as PQ
import           System.Posix.Types ( CPid )
import           GHC.IO.Exception ( ioe_location )

#if defined(mingw32_HOST_OS)
import           Control.Concurrent ( threadDelay )
#else
import           GHC.Conc           ( atomically )
import           Control.Concurrent ( threadWaitReadSTM )
#endif

data Notification = Notification
   { notificationPid     :: {-# UNPACK #-} !CPid
   , notificationChannel :: {-# UNPACK #-} !B.ByteString
   , notificationData    :: {-# UNPACK #-} !B.ByteString
   } deriving (Show, Eq)

convertNotice :: PQ.Notify -> Notification
convertNotice PQ.Notify{..}
    = Notification { notificationPid     = notifyBePid
                   , notificationChannel = notifyRelname
                   , notificationData    = notifyExtra   }

-- | Returns a single notification.   If no notifications are available,
--   'getNotification' blocks until one arrives.
--
--   It is safe to call 'getNotification' on a connection that is concurrently
--   being used for other purposes,   note however that PostgreSQL does not
--   deliver notifications while a connection is inside a transaction.

getNotification :: Connection -> IO Notification
getNotification conn = join $ withConnection conn fetch
  where
    funcName = "Database.PostgreSQL.Simple.Notification.getNotification"

    fetch c = do
        mmsg <- PQ.notifies c
        case mmsg of
          Just msg -> return (return $! convertNotice msg)
          Nothing -> do
              mfd <- PQ.socket c
              case mfd of
                Nothing  -> return (throwIO $! fdError funcName)
#if defined(mingw32_HOST_OS)
                -- threadWaitRead doesn't work for sockets on Windows, so just
                -- poll for input every second (PQconsumeInput is non-blocking).
                --
                -- We could call select(), but FFI calls can't be interrupted
                -- with async exceptions, whereas threadDelay can.
                Just _fd -> do
                  return (threadDelay 1000000 >> loop)
#else
                -- This case fixes the race condition above.   By registering
                -- our interest in the descriptor before we drop the lock,
                -- there is no opportunity for the descriptor index to be
                -- reallocated on us.
                --
                -- (That is, assuming there isn't concurrently executing
                -- code that manipulates the descriptor without holding
                -- the lock... but such a major bug is likely to exhibit
                -- itself in an at least somewhat more dramatic fashion.)
                Just fd  -> do
                  (waitRead, _) <- threadWaitReadSTM fd
                  return $ do
                    atomically waitRead `catch` (throwIO . setIOErrorLocation)
                    loop
#endif

    loop = join $ withConnection conn $ \c -> do
             void $ PQ.consumeInput c
             fetch c

    setIOErrorLocation :: IOError -> IOError
    setIOErrorLocation err = err { ioe_location = B8.unpack funcName }


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

-- | Returns the process 'CPid' of the backend server process
-- handling this connection.
--
-- The backend PID is useful for debugging purposes and for comparison
-- to NOTIFY messages (which include the PID of the notifying backend
-- process). Note that the PID belongs to a process executing on the
-- database server host, not the local host!

getBackendPID :: Connection -> IO CPid
getBackendPID conn = withConnection conn PQ.backendPID
