
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Internal.PQResultUtils
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------


module Database.PostgreSQL.Simple.Internal.PQResultUtils
    ( finishQueryWith
    , getRowWith
    ) where

import           Control.Exception as E
import           Data.ByteString (ByteString)
import           Database.PostgreSQL.Simple.FromField (ResultError(..))
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types (Query(..))
import           Database.PostgreSQL.Simple.Internal as Base
import           Database.PostgreSQL.Simple.TypeInfo
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString.Char8 as B
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict

finishQueryWith :: RowParser r -> Connection -> Query -> PQ.Result -> IO [r]
finishQueryWith parser conn q result = do
  status <- PQ.resultStatus result
  case status of
    PQ.EmptyQuery ->
        throwIO $ QueryError "query: Empty query" q
    PQ.CommandOk ->
        throwIO $ QueryError "query resulted in a command response" q
    PQ.TuplesOk -> do
        nrows <- PQ.ntuples result
        ncols <- PQ.nfields result
        forM' 0 (nrows-1) $ \row ->
            getRowWith parser row ncols conn result
    PQ.CopyOut ->
        throwIO $ QueryError "query: COPY TO is not supported" q
    PQ.CopyIn ->
        throwIO $ QueryError "query: COPY FROM is not supported" q
    PQ.BadResponse   -> throwResultError "query" result status
    PQ.NonfatalError -> throwResultError "query" result status
    PQ.FatalError    -> throwResultError "query" result status

getRowWith :: RowParser r -> PQ.Row -> PQ.Column -> Connection -> PQ.Result -> IO r
getRowWith parser row ncols conn result = do
  let rw = Row row result
  let unCol (PQ.Col x) = fromIntegral x :: Int
  okvc <- runConversion (runStateT (runReaderT (unRP parser) rw) 0) conn
  case okvc of
    Ok (val,col) | col == ncols -> return val
                 | otherwise -> do
                     vals <- forM' 0 (ncols-1) $ \c -> do
                         tinfo <- getTypeInfo conn =<< PQ.ftype result c
                         v <- PQ.getvalue result row c
                         return ( tinfo
                                , fmap ellipsis v       )
                     throw (ConversionFailed
                      (show (unCol ncols) ++ " values: " ++ show vals)
                      Nothing
                      ""
                      (show (unCol col) ++ " slots in target type")
                      "mismatch between number of columns to \
                      \convert and number in target type")
    Errors []  -> throwIO $ ConversionFailed "" Nothing "" "" "unknown error"
    Errors [x] -> throwIO x
    Errors xs  -> throwIO $ ManyErrors xs

ellipsis :: ByteString -> ByteString
ellipsis bs
    | B.length bs > 15 = B.take 10 bs `B.append` "[...]"
    | otherwise        = bs

forM' :: (Ord n, Num n) => n -> n -> (n -> IO a) -> IO [a]
forM' lo hi m = loop hi []
  where
    loop !n !as
      | n < lo = return as
      | otherwise = do
           a <- m n
           loop (n-1) (a:as)
{-# INLINE forM' #-}
