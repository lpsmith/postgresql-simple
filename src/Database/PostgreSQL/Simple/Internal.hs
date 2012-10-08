{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Internal
-- Copyright:   (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- Internal bits.  This interface is less stable and can change at any time.
-- In particular this means that while the rest of the postgresql-simple
-- package endeavors to follow the package versioning policy,  this module
-- does not.  Also, at the moment there are things in here that aren't
-- particularly internal and are exported elsewhere;  these will eventually
-- disappear from this module.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Internal where

import Prelude hiding (catch)

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent.MVar
import           Data.ByteString(ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (ord)
import           Data.Int (Int64)
import qualified Data.IntMap as IntMap
import           Data.String
import           Data.Typeable
import           Data.Word
import           Database.PostgreSQL.LibPQ(Oid(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.BuiltinTypes (BuiltinType)
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types (Query(..))
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import qualified Data.Vector as V
import           System.IO.Unsafe (unsafePerformIO)

-- | A Field represents metadata about a particular field
--
-- You don't particularly want to retain these structures for a long
-- period of time,  as they will retain the entire query result,  not
-- just the field metadata

data Field = Field {
     result   :: !PQ.Result
   , column   :: {-# UNPACK #-} !PQ.Column
   , typeinfo :: !TypeInfo
   }

data NamedOid = NamedOid { typoid  :: !PQ.Oid
                         , typname :: !ByteString
                         } deriving Show

data TypeInfo = TypeInfo { typ     :: !NamedOid
                         , typelem :: !(Maybe NamedOid)
                         } deriving Show

typename :: Field -> ByteString
typename = typname . typ . typeinfo

name :: Field -> Maybe ByteString
name Field{..} = unsafePerformIO (PQ.fname result column)

tableOid :: Field -> PQ.Oid
tableOid Field{..} = unsafePerformIO (PQ.ftable result column)

tableColumn :: Field -> Int
tableColumn Field{..} = fromCol (unsafePerformIO (PQ.ftablecol result column))
  where
    fromCol (PQ.Col x) = fromIntegral x


format :: Field -> PQ.Format
format Field{..} = unsafePerformIO (PQ.fformat result column)

typeOid :: Field -> PQ.Oid
typeOid = typoid . typ . typeinfo

data Connection = Connection {
     connectionHandle  :: {-# UNPACK #-} !(MVar PQ.Connection)
   , connectionObjects :: {-# UNPACK #-} !(MVar (IntMap.IntMap TypeInfo))
   }

data SqlType
   = Builtin BuiltinType
   | Other   Oid

data SqlError = SqlError {
     sqlState       :: ByteString
   , sqlNativeError :: Int
   , sqlErrorMsg    :: ByteString
   } deriving (Show, Typeable)

instance Exception SqlError

-- | Exception thrown if 'query' is used to perform an @INSERT@-like
-- operation, or 'execute' is used to perform a @SELECT@-like operation.
data QueryError = QueryError {
      qeMessage :: String
    , qeQuery :: Query
    } deriving (Eq, Show, Typeable)

instance Exception QueryError

data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    } deriving (Eq,Read,Show,Typeable)

-- | Default information for setting up a connection.
--
-- Defaults are as follows:
--
-- * Server on @localhost@
--
-- * Port on @5432@
--
-- * User @postgres@
--
-- * No password
--
-- * Database @postgres@
--
-- Use as in the following example:
--
-- > connect defaultConnectInfo { connectHost = "db.example.com" }

defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo {
                       connectHost = "127.0.0.1"
                     , connectPort = 5432
                     , connectUser = "postgres"
                     , connectPassword = ""
                     , connectDatabase = ""
                     }

-- | Connect with the given username to the given database. Will throw
--   an exception if it cannot connect.
connect :: ConnectInfo -> IO Connection
connect = connectPostgreSQL . postgreSQLConnectionString

-- | Attempt to make a connection based on a libpq connection string.
--   See <http://www.postgresql.org/docs/9.1/static/libpq-connect.html>
--   for more information.

connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connstr = do
    conn <- PQ.connectdb connstr
    stat <- PQ.status conn
    case stat of
      PQ.ConnectionOk -> do
          connectionHandle  <- newMVar conn
          connectionObjects <- newMVar (IntMap.empty)
          let wconn = Connection{..}
          version <- PQ.serverVersion conn
          let settings
                | version < 80200 = "SET datestyle TO ISO"
                | otherwise       = "SET standard_conforming_strings TO on;\
                                    \SET datestyle TO ISO"
          _ <- execute_ wconn settings
          return wconn
      _ -> do
          msg <- maybe "connectPostgreSQL error" id <$> PQ.errorMessage conn
          throwIO $ SqlError { sqlNativeError = -1   -- FIXME?
                             , sqlErrorMsg    = msg
                             , sqlState       = ""  }

-- | Turns a 'ConnectInfo' data structure into a libpq connection string.

postgreSQLConnectionString :: ConnectInfo -> ByteString
postgreSQLConnectionString connectInfo = fromString connstr
  where
    connstr = str "host="     connectHost
            $ num "port="     connectPort
            $ str "user="     connectUser
            $ str "password=" connectPassword
            $ str "dbname="   connectDatabase
            $ []

    str name field
      | null value = id
      | otherwise  = showString name . quote value . space
        where value = field connectInfo

    num name field
      | value <= 0 = id
      | otherwise  = showString name . shows value . space
        where value = field connectInfo

    quote str rest = '\'' : foldr delta ('\'' : rest) str
       where
         delta c cs = case c of
                        '\\' -> '\\' : '\\' : cs
                        '\'' -> '\\' : '\'' : cs
                        _    -> c : cs

    space [] = []
    space xs = ' ':xs



oid2int :: Oid -> Int
oid2int (Oid x) = fromIntegral x
{-# INLINE oid2int #-}

exec :: Connection
     -> ByteString
     -> IO PQ.Result
exec conn sql =
    withConnection conn $ \h -> do
        mres <- PQ.exec h sql
        case mres of
          Nothing -> do
            msg <- maybe "execute error" id <$> PQ.errorMessage h
            throwIO $ SqlError { sqlNativeError = -1   -- FIXME?
                               , sqlErrorMsg    = msg
                               , sqlState       = ""  }
          Just res -> do
            return res

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO Int64
execute_ conn q@(Query stmt) = do
  result <- exec conn stmt
  finishExecute conn q result

finishExecute :: Connection -> Query -> PQ.Result -> IO Int64
finishExecute _conn q result = do
    status <- PQ.resultStatus result
    case status of
      PQ.EmptyQuery -> throwIO $ QueryError "execute: Empty query" q
      PQ.CommandOk -> do
          ncols <- PQ.nfields result
          if ncols /= 0
          then throwIO $ QueryError ("execute resulted in " ++ show ncols ++
                                     "-column result") q
          else do
            nstr <- PQ.cmdTuples result
            return $ case nstr of
                       Nothing  -> 0   -- is this appropriate?
                       Just str -> toInteger str
      PQ.TuplesOk -> do
          ncols <- PQ.nfields result
          throwIO $ QueryError ("execute resulted in " ++ show ncols ++
                                 "-column result") q
      PQ.CopyOut ->
          throwIO $ QueryError "execute: COPY TO is not supported" q
      PQ.CopyIn ->
          throwIO $ QueryError "execute: COPY FROM is not supported" q
      PQ.BadResponse   -> throwResultError "execute" result status
      PQ.NonfatalError -> throwResultError "execute" result status
      PQ.FatalError    -> throwResultError "execute" result status
    where
     toInteger str = B8.foldl' delta 0 str
                where
                  delta acc c =
                    if '0' <= c && c <= '9'
                    then 10 * acc + fromIntegral (ord c - ord '0')
                    else error ("finishExecute:  not an int: " ++ B8.unpack str)

throwResultError :: ByteString -> PQ.Result -> PQ.ExecStatus -> IO a
throwResultError context result status = do
    errormsg  <- maybe "" id <$> PQ.resultErrorMessage result
    statusmsg <- PQ.resStatus status
    state     <- maybe "" id <$> PQ.resultErrorField result PQ.DiagSqlstate
    throwIO $ SqlError { sqlState = state
                       , sqlNativeError = fromEnum status
                       , sqlErrorMsg = B.concat [ context, ": ", statusmsg
                                                , ": ", errormsg ]}

disconnectedError :: SqlError
disconnectedError = SqlError {
                      sqlNativeError = -1,
                      sqlErrorMsg    = "connection disconnected",
                      sqlState       = ""
                    }

-- | Atomically perform an action with the database handle, if there is one.
withConnection :: Connection -> (PQ.Connection -> IO a) -> IO a
withConnection Connection{..} m = do
    withMVar connectionHandle $ \conn -> do
        if PQ.isNullConnection conn
          then throwIO disconnectedError
          else m conn

close :: Connection -> IO ()
close Connection{..} =
    mask $ \restore -> (do
            conn <- takeMVar connectionHandle
            restore (PQ.finish conn)
        `finally` do
            putMVar connectionHandle =<< PQ.newNullConnection
        )

newNullConnection :: IO Connection
newNullConnection = do
    connectionHandle  <- newMVar =<< PQ.newNullConnection
    connectionObjects <- newMVar IntMap.empty
    return Connection{..}

data Row = Row {
     row        :: {-# UNPACK #-} !PQ.Row
   , typeinfos  :: !(V.Vector TypeInfo)
   , rowresult  :: !PQ.Result
   }

typenames :: Row -> V.Vector ByteString
typenames = V.map (typname . typ) . typeinfos

newtype RowParser a = RP { unRP :: ReaderT Row (StateT PQ.Column Ok) a }
   deriving ( Functor, Applicative, Alternative, Monad )

getvalue :: PQ.Result -> PQ.Row -> PQ.Column -> Maybe ByteString
getvalue result row col = unsafePerformIO (PQ.getvalue result row col)

nfields :: PQ.Result -> PQ.Column
nfields result = unsafePerformIO (PQ.nfields result)
