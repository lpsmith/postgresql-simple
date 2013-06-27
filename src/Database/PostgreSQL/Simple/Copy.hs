{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Copy
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- mid-level support for COPY IN and COPY OUT.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Copy
    ( copy
    , copy_
    , getCopyData
    , putCopyData
    , putCopyEnd
    , putCopyError
    ) where

import           Control.Applicative
import           Control.Concurrent ( threadWaitRead, threadWaitWrite )
import           Control.Exception  ( throwIO )
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Int(Int64)
import qualified Data.ByteString.Char8 as B
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.Internal

copy :: ( ToRow params ) => Connection -> Query -> params -> IO ()
copy conn template qs = do
    q <- formatQuery conn template qs
    doCopy "Database.PostgreSQL.Simple.Copy.copy" conn template q

copy_ :: Connection -> Query -> IO ()
copy_ conn (Query q) = do
    doCopy "Database.PostgreSQL.Simple.Copy.copy_" conn (Query q) q

doCopy :: B.ByteString -> Connection -> Query -> B.ByteString -> IO ()
doCopy funcName conn template q = do
    result <- exec conn q
    status <- PQ.resultStatus result
    let err = throwIO $ QueryError
                  (B.unpack funcName ++ " " ++ show status)
                  template
    case status of
      PQ.EmptyQuery    -> err
      PQ.CommandOk     -> err
      PQ.TuplesOk      -> err
      PQ.CopyOut       -> return ()
      PQ.CopyIn        -> return ()
      PQ.BadResponse   -> throwResultError funcName result status
      PQ.NonfatalError -> throwResultError funcName result status
      PQ.FatalError    -> throwResultError funcName result status
{-# INLINE doCopy #-}

data CopyOutResult
   = CopyOutRow  !B.ByteString         -- ^ Data representing exactly one row
                                       --   of the result.
   | CopyOutDone {-# UNPACK #-} !Int64 -- ^ No more rows, and a count of the
                                       --   number of rows returned.

getCopyData :: Connection -> IO CopyOutResult
getCopyData conn = withConnection conn loop
  where
    funcName = "Database.PostgreSQL.Simple.Copy.getCopyData"
    errCmdStatus    = B.unpack funcName ++ ": failed to fetch command status"
    errCmdStatusFmt = B.unpack funcName ++ ": failed to parse command status"
    loop pqconn = do
#if defined(mingw32_HOST_OS)
      row <- PQ.getCopyData pqconn False
#else
      row <- PQ.getCopyData pqconn True
#endif
      case row of
        PQ.CopyOutRow rowdata -> return $! CopyOutRow rowdata
        PQ.CopyOutDone        -> do
            result  <- maybe (fail errCmdStatus) return =<< PQ.getResult pqconn
            cmdStat <- maybe (fail errCmdStatus) return =<< PQ.cmdStatus result
            let rowCount = P.string "COPY " *> P.decimal
            case P.parseOnly (rowCount <* P.endOfInput) cmdStat of
              Left  _ -> fail errCmdStatusFmt
              Right n -> return $! CopyOutDone n
#if defined(mingw32_HOST_OS)
        PQ.CopyOutWouldBlock -> do
            fail (B.unpack funcName ++ ": the impossible happened")
#else
        PQ.CopyOutWouldBlock  -> do
            mfd <- PQ.socket pqconn
            case mfd of
              Nothing -> throwIO (fdError funcName)
              Just fd -> do
                  threadWaitRead fd
                  _ <- PQ.consumeInput pqconn
                  loop pqconn
#endif
        PQ.CopyOutError       -> do
            mmsg <- PQ.errorMessage pqconn
            throwIO SqlError {
                          sqlState       = "",
                          sqlExecStatus  = FatalError,
                          sqlErrorMsg    = maybe "" id mmsg,
                          sqlErrorDetail = "",
                          sqlErrorHint   = funcName
                        }

putCopyData :: Connection -> B.ByteString -> IO ()
putCopyData conn dat =
    doCopyIn "Database.PostgreSQL.Simple.Copy.putCopyData"
             (\c -> PQ.putCopyData c dat)
             conn

putCopyEnd :: Connection -> IO ()
putCopyEnd conn = do
    doCopyIn "Database.PostgreSQL.Simple.Copy.putCopyEnd"
             (\c -> PQ.putCopyEnd c Nothing)
             conn

putCopyError :: Connection -> B.ByteString -> IO ()
putCopyError conn err = do
    doCopyIn "Database.PostgreSQL.Simple.Copy.putCopyError"
             (\c -> PQ.putCopyEnd c (Just err))
             conn

doCopyIn :: B.ByteString -> (PQ.Connection -> IO PQ.CopyInResult)
         -> Connection -> IO ()
doCopyIn funcName action conn = withConnection conn loop
  where
    loop pqconn = do
      stat <- action pqconn
      case stat of
        PQ.CopyInOk    -> return ()
        PQ.CopyInError -> do
            mmsg <- PQ.errorMessage pqconn
            throwIO SqlError {
                      sqlState = "",
                      sqlExecStatus  = FatalError,
                      sqlErrorMsg    = maybe "" id mmsg,
                      sqlErrorDetail = "",
                      sqlErrorHint   = funcName
                    }
        PQ.CopyInWouldBlock -> do
            mfd <- PQ.socket pqconn
            case mfd of
              Nothing -> throwIO (fdError funcName)
              Just fd -> do
                  threadWaitWrite fd
                  loop pqconn
{-# INLINE doCopyIn #-}
