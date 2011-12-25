{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Database.HDBC.PostgreSQL.Notification
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Notification
     ( Notification(..)
     , getNotification
     ) where

import           Control.Concurrent ( threadWaitRead )
import           Control.Concurrent.MVar ( takeMVar, putMVar )
import qualified Data.ByteString as B
import           Database.PostgreSQL.Simple.Implementation
import qualified Database.PostgreSQL.LibPQ as PQ
import           System.Posix.Types ( CPid )

data Notification  = Notification
                      { notificationPid     :: CPid
                      , notificationChannel :: B.ByteString
                      , notificationData    :: B.ByteString
                      }

errfd   = "Database.PostgreSQL.Simple.Notification.getNotification: \
          \failed to fetch file descriptor"
errconn = "Database.PostgreSQL.Simple.Notification.getNotification: \
          \not connected"

lockConn :: Connection -> IO (PQ.Connection)
lockConn Connection{..} = do
    mconn <- takeMVar connectionHandle
    case mconn of
      Nothing   -> do
                      putMVar connectionHandle mconn
                      fail errconn
      Just conn -> return conn

unlockConn :: Connection -> PQ.Connection -> IO ()
unlockConn Connection{..} c = putMVar connectionHandle (Just c)

getNotification :: Connection -> IO Notification
getNotification conn = do
    c <- lockConn conn
    loop conn c
  where
    -- now, I believe the only ways that this code throws an exception is:
    --    1.  lockConn/unlockConn when we are blocked on a GC'd MVar
    --    2.  threadWaitRead when closeFdWith gets called
    --    3.  and if we raise it ourself
    -- If 1 happens, then it doesn't matter if the MVar is locked or not,
    -- and if 2 or 3 happens then the connection should be unlocked.
    loop conn c = do
        mmsg <- PQ.notifies c
        case mmsg of
          Nothing -> do
              mfd <- PQ.socket c
              unlockConn conn c
              case mfd of
                Nothing -> fail errfd
                Just fd -> do
                              threadWaitRead fd
                              c <- lockConn conn
                              _ <- PQ.consumeInput c
                                -- FIXME? error handling
                              loop conn c
          Just PQ.Notify{..} -> do
              unlockConn conn c
              return Notification { notificationPid     = notifyBePid
                                  , notificationChannel = notifyRelname
                                  , notificationData    = notifyExtra   }
