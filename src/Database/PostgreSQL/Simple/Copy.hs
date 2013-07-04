{-# LANGUAGE CPP, DeriveDataTypeable #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Copy
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- mid-level support for COPY IN and COPY OUT.   See
-- <http://www.postgresql.org/docs/9.2/static/sql-copy.html> for
-- more information.
--
-- To use this binding,  first call 'copy' with a @COPY FROM STDIN@
-- or @COPY TO STDOUT@ query as documented in the link above.  Then
-- call @getCopyData@ repeatedly until it returns 'CopyOutDone' in
-- the former case,  or in the latter, call @putCopyData@ repeatedly
-- and then finish by calling either @putCopyEnd@ to proceed or 
-- @putCopyError@ to abort.
--
-- You cannot issue another query on the same connection while a copy
-- is ongoing; this will result in an exception.   It is harmless to
-- concurrently call @getNotification@ on a connection while it is in
-- a copy in or copy out state,  however be aware that current versions
-- of the PostgreSQL backend will not deliver notifications to a client
-- while a transaction is ongoing.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Copy
    ( copy
    , copy_
    , CopyOutResult(..)
    , getCopyData
    , putCopyData
    , putCopyEnd
    , putCopyError
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception  ( throwIO )
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Typeable(Typeable)
import           Data.Int(Int64)
import qualified Data.ByteString.Char8 as B
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.Internal

-- | Issue a query that changes a connection's state to @CopyIn@ 
--   (via a @COPY FROM STDIN@ query) or @CopyOut@ (via @COPY TO STDOUT@)
--   query.  Performs parameter subsitution.

copy :: ( ToRow params ) => Connection -> Query -> params -> IO ()
copy conn template qs = do
    q <- formatQuery conn template qs
    doCopy "Database.PostgreSQL.Simple.Copy.copy" conn template q

-- | Issue a query that changes a connection's state to @CopyIn@ 
--   (via a @COPY FROM STDIN@ query) or @CopyOut@ (via @COPY TO STDOUT@)
--   query.  Does not perform parameter subsitution.

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

data CopyOutResult
   = CopyOutRow  !B.ByteString         -- ^ Data representing either exactly 
                                       --   one row of the result,  or header
                                       --   or footer data depending on format.
   | CopyOutDone {-# UNPACK #-} !Int64 -- ^ No more rows, and a count of the
                                       --   number of rows returned.
     deriving (Eq, Typeable, Show)

-- | A connection must be in the @CopyOut@ state in order to call this
--   function,  via a @COPY TO STDOUT@ query.  If this returns a 'CopyOutRow', 
--   the connection remains in the @CopyOut@ state, if it returns 'CopyOutDone',
--   then the connection has reverted to the ready state.

getCopyData :: Connection -> IO CopyOutResult
getCopyData conn = withConnection conn loop
  where
    funcName = "Database.PostgreSQL.Simple.Copy.getCopyData"
    loop pqconn = do
#if defined(mingw32_HOST_OS)
      row <- PQ.getCopyData pqconn False
#else
      row <- PQ.getCopyData pqconn True
#endif
      case row of
        PQ.CopyOutRow rowdata -> return $! CopyOutRow rowdata
        PQ.CopyOutDone -> CopyOutDone <$> getCopyCommandTag funcName pqconn
#if defined(mingw32_HOST_OS)
        PQ.CopyOutWouldBlock -> do
            fail (B.unpack funcName ++ ": the impossible happened")
#else
        PQ.CopyOutWouldBlock -> do
            mfd <- PQ.socket pqconn
            case mfd of
              Nothing -> throwIO (fdError funcName)
              Just fd -> do
                  threadWaitRead fd
                  _ <- PQ.consumeInput pqconn
                  loop pqconn
#endif
        PQ.CopyOutError -> do
            mmsg <- PQ.errorMessage pqconn
            throwIO SqlError {
                          sqlState       = "",
                          sqlExecStatus  = FatalError,
                          sqlErrorMsg    = maybe "" id mmsg,
                          sqlErrorDetail = "",
                          sqlErrorHint   = funcName
                        }

-- | A connection must be in the @CopyIn@ state in order to call this
--   function,  via a @COPY FROM STDIN@ query.  The connection remains
--   in a @CopyIn@ state after this function is called.   Note that 
--   the data does not need to represent a single row,  or even an
--   integral number of rows.  The net result of 
--   @putCopyData conn a >> putCopyData conn b@
--   is the same as @putCopyData conn c@ whenever @c == BS.append a b@.

putCopyData :: Connection -> B.ByteString -> IO ()
putCopyData conn dat = withConnection conn $ \pqconn -> do
    doCopyIn funcName (\c -> PQ.putCopyData c dat) pqconn
  where
    funcName = "Database.PostgreSQL.Simple.Copy.putCopyData"


-- | A connection must be in the @CopyIn@ state in order to call this
--   function,  via a @COPY FROM STDIN@ query.  Completes the COPY IN
--   operation,  changing the connection's state back to normal.
--   Returns the number of rows processed.

putCopyEnd :: Connection -> IO Int64
putCopyEnd conn = withConnection conn $ \pqconn -> do
    doCopyIn funcName (\c -> PQ.putCopyEnd c Nothing) pqconn
    getCopyCommandTag funcName pqconn
  where
    funcName = "Database.PostgreSQL.Simple.Copy.putCopyEnd"

-- | A connection must be in the @CopyIn@ state in order to call this
--   function,  via a @COPY FROM STDIN@ query.  Aborts the COPY IN
--   operation,  changing the connection's state back to normal.

putCopyError :: Connection -> B.ByteString -> IO ()
putCopyError conn err = withConnection conn $ \pqconn -> do
    doCopyIn funcName (\c -> PQ.putCopyEnd c (Just err)) pqconn
  where
    funcName = "Database.PostgreSQL.Simple.Copy.putCopyError"


doCopyIn :: B.ByteString -> (PQ.Connection -> IO PQ.CopyInResult)
         -> PQ.Connection -> IO ()
doCopyIn funcName action = loop
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

getCopyCommandTag :: B.ByteString -> PQ.Connection -> IO Int64
getCopyCommandTag funcName pqconn = do
    result  <- maybe (fail errCmdStatus) return =<< PQ.getResult pqconn
    cmdStat <- maybe (fail errCmdStatus) return =<< PQ.cmdStatus result
    let rowCount =   P.string "COPY " *> (P.decimal <* P.endOfInput)
                 <|> (P.endOfInput *> pure 0)
    case P.parseOnly rowCount cmdStat of
      Left  _ -> fail errCmdStatusFmt
      Right n -> return $! n
  where
    errCmdStatus    = B.unpack funcName ++ ": failed to fetch command status"
    errCmdStatusFmt = B.unpack funcName ++ ": failed to parse command status"
