{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Internal
-- Copyright:   (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- Internal bits.  This interface is less stable and can change at any time.
-- Also,  at the moment there are things in here that aren't particularly
-- internal and are exported elsewhere;  these will eventually disappear
-- from this module.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Internal where

import Prelude hiding (catch)

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent.MVar
import           Data.ByteString(ByteString)
import qualified Data.IntMap as IntMap
import           Data.String
import           Data.Typeable
import           Data.Word
import           Database.PostgreSQL.LibPQ(Oid(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.BuiltinTypes (BuiltinType)
import           System.IO.Unsafe (unsafePerformIO)

-- | A Field represents metadata about a particular field
--
-- You don't particularly want to retain these structures for a long
-- period of time,  as they will retain the entire query result,  not
-- just the field metadata

data Field = Field {
     result   :: PQ.Result
   , column   :: PQ.Column
   , typename :: ByteString
   }

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
typeOid Field{..} = unsafePerformIO (PQ.ftype result column)


data Connection = Connection {
     connectionHandle  :: {-# UNPACK #-} !(MVar PQ.Connection)
   , connectionObjects :: {-# UNPACK #-} !(MVar (IntMap.IntMap ByteString))
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

data ConnectInfo = ConnectInfo {
      connectHost :: Maybe String
    , connectPort :: Maybe Word16
    , connectUser :: Maybe String
    , connectPassword :: Maybe String
    , connectDatabase :: Maybe String
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
                       connectHost = Just "127.0.0.1"
                     , connectPort = Just 5432
                     , connectUser = Just "postgres"
                     , connectPassword = Nothing
                     , connectDatabase = Nothing
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
          return Connection{..}
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

    str name field = case field connectInfo of
      Nothing    -> id
      Just value -> (name ++) . quote value . space

    num name field = case field connectInfo of
      Nothing    -> id
      Just value -> (name ++) . shows value . space

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

data RawResult = RawResult { rawField :: Field, rawData :: Maybe ByteString }
