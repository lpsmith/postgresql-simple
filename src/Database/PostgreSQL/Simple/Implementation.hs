{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
module Database.PostgreSQL.Simple.Implementation where

import Prelude hiding (catch)

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class (MonadIO(liftIO))
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


data Status err a = Fail err | Success a deriving(Eq, Ord, Show, Functor)

instance Applicative (Status err) where
   pure = Success
   (Success f) <*> (Success a) = Success (f a)
   (Success _) <*> (Fail  err) = Fail err
   (Fail  err) <*> _           = Fail err

instance Monad (Status err) where
   return = pure
   (Success a) >>= f = f a
   (Fail  err) >>= _ = Fail err


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
     connectionHandle  :: MVar (Maybe PQ.Connection)
   , connectionObjects :: MVar (IntMap.IntMap ByteString)
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
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    } deriving (Eq,Read,Show,Typeable)

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
connect :: MonadIO m => ConnectInfo -> m Connection
connect = connectPostgreSQL . postgreSQLConnectionString

connectPostgreSQL :: MonadIO m => ByteString -> m Connection
connectPostgreSQL connstr = liftIO $ do
    conn <- PQ.connectdb connstr
    connectionHandle <- newMVar (Just conn)
    connectionObjects <- newMVar (IntMap.empty)
    return Connection{..}

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
      | otherwise  = (name ++) . quote value . space
        where value = field connectInfo

    num name field
      | value <= 0 = id
      | otherwise  = (name ++) . (show value ++) . space
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
            msg <- PQ.errorMessage h
            -- FIXME better error message
            fail ("Postgres Error:  " ++ show msg)
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
  withMVar connectionHandle $ \h -> do
    case h of
      Just h -> m h
      -- TODO: Use extensible exceptions.
      Nothing -> throwIO disconnectedError

close :: Connection -> IO ()
close Connection{..} = do
    mconn <- takeMVar connectionHandle
    case mconn of
         Just conn -> PQ.finish conn
         Nothing   -> return ()
      `finally` putMVar connectionHandle Nothing

data RawResult = RawResult { rawField :: Field, rawData :: Maybe ByteString }
