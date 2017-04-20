------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Cursor
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2017 Bardur Arantsson
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Cursor
    (
    -- * Types
      Cursor
    -- * Cursor management
    , declareCursor
    , declareTemporaryCursor
    , closeCursor
    -- * Fetching rows
    , fetchForward
    , fetchForwardWithParser
    ) where

import           Data.ByteString.Builder (byteString, intDec)
import           Control.Applicative ((<$>))
import           Control.Exception as E
import           Control.Monad (unless, void)
import           Data.ByteString (ByteString)
import           Data.Monoid (mconcat)
import           Database.PostgreSQL.Simple.Compat ((<>), toByteString)
import           Database.PostgreSQL.Simple.FromRow (FromRow(..))
import           Database.PostgreSQL.Simple.ToField (Action(..))
import           Database.PostgreSQL.Simple.Types (Query(..))
import           Database.PostgreSQL.Simple.Internal as Base
import           Database.PostgreSQL.Simple.Internal.PQResultUtils
import           Database.PostgreSQL.Simple.Transaction
import qualified Database.PostgreSQL.LibPQ as PQ

-- | Cursor within a transaction.
data Cursor = Cursor !ByteString !Connection

-- | Declare a cursor with the given name on the given connection. The
-- cursor only remains valid as long as the current transaction
-- exists.
declareCursor :: Connection -> ByteString -> Query -> IO Cursor
declareCursor conn name q = do
  name <- toByteString <$> extract <$> escapeIdentifier conn name
  void $ execute_ conn $ mconcat ["DECLARE ", Query name, " NO SCROLL CURSOR FOR ", q]
  return $ Cursor name conn
  where
    extract = either (fmtError "declareCursor" q [EscapeIdentifier name]) byteString

-- | Declare a temporary cursor. The cursor is given a
-- unique name for the given connection.
declareTemporaryCursor :: Connection -> Query -> IO Cursor
declareTemporaryCursor conn q = do
  (Query name) <- newTempName conn
  declareCursor conn name q

-- | Close the given cursor.
closeCursor :: Cursor -> IO ()
closeCursor (Cursor name conn) =
  (void $ execute_ conn ("CLOSE " <> Query name)) `E.catch` \ex ->
     -- Don't throw exception if CLOSE failed because the transaction is
     -- aborted.  Otherwise, it will throw away the original error.
     unless (isFailedTransactionError ex) $ throwIO ex

-- | Fetch a chunk of rows, calling the supplied fold-like function
-- on each row as it is received. In case the cursor is exhausted,
-- a 'Left' value is returned, otherwise a 'Right' value is returned.
fetchForwardWithParser :: Cursor -> RowParser r -> Int -> (a -> r -> IO a) -> a -> IO (Either a a)
fetchForwardWithParser (Cursor name conn) parser chunkSize f a0 = do
  let q = toByteString (byteString "FETCH FORWARD "
                          <> intDec chunkSize
                          <> byteString " FROM "
                          <> byteString name)
  result <- exec conn q
  status <- PQ.resultStatus result
  case status of
      PQ.TuplesOk -> do
          nrows <- PQ.ntuples result
          ncols <- PQ.nfields result
          if nrows > 0
          then do
              let inner a row = do
                    x <- getRowWith parser row ncols conn result
                    f a x
              Right <$> foldM' inner a0 0 (nrows - 1)
          else
            return $ Left a0
      _   -> throwResultError "fetchForward" result status

-- | Fetch a chunk of rows, calling the supplied fold-like function
-- on each row as it is received. In case the cursor is exhausted,
-- a 'Left' value is returned, otherwise a 'Right' value is returned.
fetchForward :: FromRow r => Cursor -> Int -> (a -> r -> IO a) -> a -> IO (Either a a)
fetchForward cursor = fetchForwardWithParser cursor fromRow


foldM' :: (Ord n, Num n) => (a -> n -> IO a) -> a -> n -> n -> IO a
foldM' f a lo hi = loop a lo
  where
    loop a !n
      | n > hi = return a
      | otherwise = do
           a' <- f a n
           loop a' (n+1)
{-# INLINE foldM' #-}
