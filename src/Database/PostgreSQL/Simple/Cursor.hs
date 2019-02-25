{-# LANGUAGE OverloadedStrings  #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Cursor
-- Copyright:   (c) 2011-2012 Leon P Smith
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
    , closeCursor
    -- * Folding over rows from a cursor
    , foldForward
    , foldForwardWithParser
    ) where

import           Data.ByteString.Builder (intDec)
import           Control.Applicative ((<$>))
import           Control.Exception as E
import           Control.Monad (unless, void)
import           Data.Monoid (mconcat)
import           Database.PostgreSQL.Simple.Compat ((<>), toByteString)
import           Database.PostgreSQL.Simple.FromRow (FromRow(..))
import           Database.PostgreSQL.Simple.Types (Query(..))
import           Database.PostgreSQL.Simple.Internal as Base hiding (result, row)
import           Database.PostgreSQL.Simple.Internal.PQResultUtils
import           Database.PostgreSQL.Simple.Transaction
import qualified Database.PostgreSQL.LibPQ as PQ

-- | Cursor within a transaction.
data Cursor = Cursor !Query !Connection

-- | Declare a temporary cursor. The cursor is given a
-- unique name for the given connection.
declareCursor :: Connection -> Query -> IO Cursor
declareCursor conn q = do
  name <- newTempName conn
  void $ execute_ conn $ mconcat ["DECLARE ", name, " NO SCROLL CURSOR FOR ", q]
  return $ Cursor name conn

-- | Close the given cursor.
closeCursor :: Cursor -> IO ()
closeCursor (Cursor name conn) =
  (void $ execute_ conn ("CLOSE " <> name)) `E.catch` \ex ->
     -- Don't throw exception if CLOSE failed because the transaction is
     -- aborted.  Otherwise, it will throw away the original error.
     unless (isFailedTransactionError ex) $ throwIO ex

-- | Fold over a chunk of rows from the given cursor, calling the
-- supplied fold-like function on each row as it is received. In case
-- the cursor is exhausted, a 'Left' value is returned, otherwise a
-- 'Right' value is returned.
foldForwardWithParser :: Cursor -> RowParser r -> Int -> (a -> r -> IO a) -> a -> IO (Either a a)
foldForwardWithParser (Cursor name conn) parser chunkSize f a0 = do
  let q = "FETCH FORWARD "
            <> (toByteString $ intDec chunkSize)
            <> " FROM "
            <> fromQuery name
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
      _   -> throwResultError "foldForwardWithParser" result status

-- | Fold over a chunk of rows, calling the supplied fold-like function
-- on each row as it is received. In case the cursor is exhausted,
-- a 'Left' value is returned, otherwise a 'Right' value is returned.
foldForward :: FromRow r => Cursor -> Int -> (a -> r -> IO a) -> a -> IO (Either a a)
foldForward cursor = foldForwardWithParser cursor fromRow


foldM' :: (Ord n, Num n) => (a -> n -> IO a) -> a -> n -> n -> IO a
foldM' f a lo hi = loop a lo
  where
    loop x !n
      | n > hi = return x
      | otherwise = do
           x' <- f x n
           loop x' (n+1)
{-# INLINE foldM' #-}
