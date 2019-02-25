{-# LANGUAGE  CPP, BangPatterns, DoAndIfThenElse, RecordWildCards  #-}
{-# LANGUAGE  DeriveDataTypeable, DeriveGeneric                    #-}
{-# LANGUAGE  GeneralizedNewtypeDeriving                           #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Internal
-- Copyright:   (c) 2011-2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
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

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent.MVar
import           Control.Monad(MonadPlus(..))
import           Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Builder ( Builder, byteString )
import           Data.Char (ord)
import           Data.Int (Int64)
import qualified Data.IntMap as IntMap
import           Data.IORef
import           Data.Maybe(fromMaybe)
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable
import           Data.Word
import           Database.PostgreSQL.LibPQ(Oid(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.LibPQ(ExecStatus(..))
import           Database.PostgreSQL.Simple.Compat ( toByteString )
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.ToField (Action(..), inQuotes)
import           Database.PostgreSQL.Simple.Types (Query(..))
import           Database.PostgreSQL.Simple.TypeInfo.Types(TypeInfo)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           GHC.Generics
import           GHC.IO.Exception
#if !defined(mingw32_HOST_OS)
import           Control.Concurrent(threadWaitRead, threadWaitWrite)
#endif

-- | A Field represents metadata about a particular field
--
-- You don't particularly want to retain these structures for a long
-- period of time,  as they will retain the entire query result,  not
-- just the field metadata

data Field = Field {
     result   :: !PQ.Result
   , column   :: {-# UNPACK #-} !PQ.Column
   , typeOid  :: {-# UNPACK #-} !PQ.Oid
     -- ^ This returns the type oid associated with the column.  Analogous
     --   to libpq's @PQftype@.
   }

type TypeInfoCache = IntMap.IntMap TypeInfo

data Connection = Connection {
     connectionHandle  :: {-# UNPACK #-} !(MVar PQ.Connection)
   , connectionObjects :: {-# UNPACK #-} !(MVar TypeInfoCache)
   , connectionTempNameCounter :: {-# UNPACK #-} !(IORef Int64)
   } deriving (Typeable)

instance Eq Connection where
   x == y = connectionHandle x == connectionHandle y

data SqlError = SqlError {
     sqlState       :: ByteString
   , sqlExecStatus  :: ExecStatus
   , sqlErrorMsg    :: ByteString
   , sqlErrorDetail :: ByteString
   , sqlErrorHint   :: ByteString
   } deriving (Eq, Show, Typeable)

fatalError :: ByteString -> SqlError
fatalError msg = SqlError "" FatalError msg "" ""

instance Exception SqlError

-- | Exception thrown if 'query' is used to perform an @INSERT@-like
-- operation, or 'execute' is used to perform a @SELECT@-like operation.
data QueryError = QueryError {
      qeMessage :: String
    , qeQuery :: Query
    } deriving (Eq, Show, Typeable)

instance Exception QueryError

-- | Exception thrown if a 'Query' could not be formatted correctly.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [ByteString]
    } deriving (Eq, Show, Typeable)

instance Exception FormatError

data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    } deriving (Generic,Eq,Read,Show,Typeable)

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
--   See <https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING>
--   for more information.  Also note that environment variables also affect
--   parameters not provided, parameters provided as the empty string, and a
--   few other things; see
--   <https://www.postgresql.org/docs/9.5/static/libpq-envars.html>
--   for details.  Here is an example with some of the most commonly used
--   parameters:
--
-- > host='db.somedomain.com' port=5432 ...
--
--   This attempts to connect to @db.somedomain.com:5432@.  Omitting the port
--   will normally default to 5432.
--
--   On systems that provide unix domain sockets,  omitting the host parameter
--   will cause libpq to attempt to connect via unix domain sockets.
--   The default filesystem path to the socket is constructed from the
--   port number and the @DEFAULT_PGSOCKET_DIR@ constant defined in the
--   @pg_config_manual.h@ header file.  Connecting via unix sockets tends
--   to use the @peer@ authentication method, which is very secure and
--   does not require a password.
--
--   On Windows and other systems without unix domain sockets, omitting
--   the host will default to @localhost@.
--
-- > ... dbname='postgres' user='postgres' password='secret \' \\ pw'
--
--   This attempts to connect to a database named @postgres@ with
--   user @postgres@ and password @secret \' \\ pw@.  Backslash
--   characters will have to be double-quoted in literal Haskell strings,
--   of course.  Omitting @dbname@ and @user@ will both default to the
--   system username that the client process is running as.
--
--   Omitting @password@ will default to an appropriate password found
--   in the @pgpass@ file,  or no password at all if a matching line is
--   not found.  The path of the @pgpass@ file may be specified by setting
--   the @PGPASSFILE@ environment variable. See
--   <https://www.postgresql.org/docs/9.5/static/libpq-pgpass.html> for
--   more information regarding this file.
--
--   As all parameters are optional and the defaults are sensible,  the
--   empty connection string can be useful for development and
--   exploratory use,  assuming your system is set up appropriately.
--
--   On Unix,  such a setup would typically consist of a local
--   postgresql server listening on port 5432,  as well as a system user,
--   database user, and database sharing a common name,  with permissions
--   granted to the user on the database.
--
--   On Windows,  in addition you will either need @pg_hba.conf@
--   to specify the use of the @trust@ authentication method for
--   the connection,  which may not be appropriate for multiuser
--   or production machines, or you will need to use a @pgpass@ file
--   with the @password@ or @md5@ authentication methods.
--
--   See <https://www.postgresql.org/docs/9.5/static/client-authentication.html>
--   for more information regarding the authentication process.
--
--   SSL/TLS will typically "just work" if your postgresql server supports or
--   requires it.  However,  note that libpq is trivially vulnerable to a MITM
--   attack without setting additional SSL connection parameters.  In
--   particular,  @sslmode@ needs to be set to @require@, @verify-ca@, or
--   @verify-full@ in order to perform certificate validation.  When @sslmode@
--   is @require@,  then you will also need to specify a @sslrootcert@ file,
--   otherwise no validation of the server's identity will be performed.
--   Client authentication via certificates is also possible via the
--   @sslcert@ and @sslkey@ parameters.   See
--   <https://www.postgresql.org/docs/9.5/static/libpq-ssl.html>
--   for detailed information regarding libpq and SSL.

connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connstr = do
    conn <- connectdb connstr
    stat <- PQ.status conn
    case stat of
      PQ.ConnectionOk -> do
          connectionHandle  <- newMVar conn
          connectionObjects <- newMVar (IntMap.empty)
          connectionTempNameCounter <- newIORef 0
          let wconn = Connection{..}
          version <- PQ.serverVersion conn
          let settings
                | version < 80200 = "SET datestyle TO ISO;SET client_encoding TO UTF8"
                | otherwise       = "SET datestyle TO ISO;SET client_encoding TO UTF8;SET standard_conforming_strings TO on"
          _ <- execute_ wconn settings
          return wconn
      _ -> do
          msg <- maybe "connectPostgreSQL error" id <$> PQ.errorMessage conn
          throwIO $ fatalError msg

connectdb :: ByteString -> IO PQ.Connection
#if defined(mingw32_HOST_OS)
connectdb = PQ.connectdb
#else
connectdb conninfo = do
    conn <- PQ.connectStart conninfo
    loop conn
  where
    funcName = "Database.PostgreSQL.Simple.connectPostgreSQL"
    loop conn = do
      status <- PQ.connectPoll conn
      case status of
        PQ.PollingFailed  -> throwLibPQError conn "connection failed"
        PQ.PollingReading -> do
                                mfd <- PQ.socket conn
                                case mfd of
                                  Nothing -> throwIO $! fdError funcName
                                  Just fd -> do
                                      threadWaitRead fd
                                      loop conn
        PQ.PollingWriting -> do
                                mfd <- PQ.socket conn
                                case mfd of
                                  Nothing -> throwIO $! fdError funcName
                                  Just fd -> do
                                      threadWaitWrite fd
                                      loop conn
        PQ.PollingOk      -> return conn

#endif

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
      | otherwise  = showString name . addQuotes value . space
        where value = field connectInfo

    num name field
      | value <= 0 = id
      | otherwise  = showString name . shows value . space
        where value = field connectInfo

    addQuotes s rest = '\'' : foldr delta ('\'' : rest) s
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
#if defined(mingw32_HOST_OS)
exec conn sql =
    withConnection conn $ \h -> do
        mres <- PQ.exec h sql
        case mres of
          Nothing  -> throwLibPQError h "PQexec returned no results"
          Just res -> return res
#else
exec conn sql =
    withConnection conn $ \h -> do
        success <- PQ.sendQuery h sql
        if success
        then awaitResult h Nothing
        else throwLibPQError h "PQsendQuery failed"
  where
    awaitResult h mres = do
        mfd <- PQ.socket h
        case mfd of
          Nothing -> throwIO $! fdError "Database.PostgreSQL.Simple.Internal.exec"
          Just fd -> do
             threadWaitRead fd
             _ <- PQ.consumeInput h  -- FIXME?
             getResult h mres

    getResult h mres = do
        isBusy <- PQ.isBusy h
        if isBusy
        then awaitResult h mres
        else do
          mres' <- PQ.getResult h
          case mres' of
            Nothing -> case mres of
                         Nothing  -> throwLibPQError h "PQgetResult returned no results"
                         Just res -> return res
            Just res -> do
                status <- PQ.resultStatus res
                case status of
                   -- FIXME: handle PQ.CopyBoth and PQ.SingleTuple
                   PQ.EmptyQuery    -> getResult h mres'
                   PQ.CommandOk     -> getResult h mres'
                   PQ.TuplesOk      -> getResult h mres'
                   PQ.CopyOut       -> return res
                   PQ.CopyIn        -> return res
                   PQ.BadResponse   -> getResult h mres'
                   PQ.NonfatalError -> getResult h mres'
                   PQ.FatalError    -> getResult h mres'
#endif

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Query -> IO Int64
execute_ conn q@(Query stmt) = do
  result <- exec conn stmt
  finishExecute conn q result

finishExecute :: Connection -> Query -> PQ.Result -> IO Int64
finishExecute _conn q result = do
    status <- PQ.resultStatus result
    case status of
      -- FIXME: handle PQ.CopyBoth and PQ.SingleTuple
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
                       Just str -> mkInteger str
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
     mkInteger str = B8.foldl' delta 0 str
                where
                  delta acc c =
                    if '0' <= c && c <= '9'
                    then 10 * acc + fromIntegral (ord c - ord '0')
                    else error ("finishExecute:  not an int: " ++ B8.unpack str)

throwResultError :: ByteString -> PQ.Result -> PQ.ExecStatus -> IO a
throwResultError _ result status = do
    errormsg  <- fromMaybe "" <$>
                 PQ.resultErrorField result PQ.DiagMessagePrimary
    detail    <- fromMaybe "" <$>
                 PQ.resultErrorField result PQ.DiagMessageDetail
    hint      <- fromMaybe "" <$>
                 PQ.resultErrorField result PQ.DiagMessageHint
    state'    <- maybe "" id <$> PQ.resultErrorField result PQ.DiagSqlstate
    throwIO $ SqlError { sqlState = state'
                       , sqlExecStatus = status
                       , sqlErrorMsg = errormsg
                       , sqlErrorDetail = detail
                       , sqlErrorHint = hint }

disconnectedError :: SqlError
disconnectedError = fatalError "connection disconnected"

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
    connectionTempNameCounter <- newIORef 0
    return Connection{..}

data Row = Row {
     row        :: {-# UNPACK #-} !PQ.Row
   , rowresult  :: !PQ.Result
   }

newtype RowParser a = RP { unRP :: ReaderT Row (StateT PQ.Column Conversion) a }
   deriving ( Functor, Applicative, Alternative, Monad )

liftRowParser :: IO a -> RowParser a
liftRowParser = RP . lift . lift . liftConversion

newtype Conversion a = Conversion { runConversion :: Connection -> IO (Ok a) }

liftConversion :: IO a -> Conversion a
liftConversion m = Conversion (\_ -> Ok <$> m)

instance Functor Conversion where
   fmap f m = Conversion $ \conn -> (fmap . fmap) f (runConversion m conn)

instance Applicative Conversion where
   pure a    = Conversion $ \_conn -> pure (pure a)
   mf <*> ma = Conversion $ \conn -> do
                   okf <- runConversion mf conn
                   case okf of
                     Ok f -> (fmap . fmap) f (runConversion ma conn)
                     Errors errs -> return (Errors errs)

instance Alternative Conversion where
   empty     = Conversion $ \_conn -> pure empty
   ma <|> mb = Conversion $ \conn -> do
                   oka <- runConversion ma conn
                   case oka of
                     Ok _     -> return oka
                     Errors _ -> (oka <|>) <$> runConversion mb conn

instance Monad Conversion where
#if !(MIN_VERSION_base(4,8,0))
   return = pure
#endif
   m >>= f = Conversion $ \conn -> do
                 oka <- runConversion m conn
                 case oka of
                   Ok a -> runConversion (f a) conn
                   Errors err -> return (Errors err)

instance MonadPlus Conversion where
   mzero = empty
   mplus = (<|>)

conversionMap :: (Ok a -> Ok b) -> Conversion a -> Conversion b
conversionMap f m = Conversion $ \conn -> f <$> runConversion m conn

conversionError :: Exception err => err -> Conversion a
conversionError err = Conversion $ \_ -> return (Errors [toException err])

newTempName :: Connection -> IO Query
newTempName Connection{..} = do
    !n <- atomicModifyIORef connectionTempNameCounter
          (\n -> let !n' = n+1 in (n', n'))
    return $! Query $ B8.pack $ "temp" ++ show n

-- FIXME?  What error should getNotification and getCopyData throw?
fdError :: ByteString -> IOError
fdError funcName = IOError {
                     ioe_handle      = Nothing,
                     ioe_type        = ResourceVanished,
                     ioe_location    = B8.unpack funcName,
                     ioe_description = "failed to fetch file descriptor",
                     ioe_errno       = Nothing,
                     ioe_filename    = Nothing
                   }


libPQError :: ByteString -> IOError
libPQError desc = IOError {
                    ioe_handle      = Nothing,
                    ioe_type        = OtherError,
                    ioe_location    = "libpq",
                    ioe_description = B8.unpack desc,
                    ioe_errno       = Nothing,
                    ioe_filename    = Nothing
                  }

throwLibPQError :: PQ.Connection -> ByteString -> IO a
throwLibPQError conn default_desc = do
  msg <- maybe default_desc id <$> PQ.errorMessage conn
  throwIO $! libPQError msg


fmtError :: String -> Query -> [Action] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map twiddle xs
                    }
  where twiddle (Plain b)            = toByteString b
        twiddle (Escape s)           = s
        twiddle (EscapeByteA s)      = s
        twiddle (EscapeIdentifier s) = s
        twiddle (Many ys)            = B.concat (map twiddle ys)

fmtErrorBs :: Query -> [Action] -> ByteString -> a
fmtErrorBs q xs msg = fmtError (T.unpack $ TE.decodeUtf8 msg) q xs

-- | Quote bytestring or throw 'FormatError'
quote :: Query -> [Action] -> Either ByteString ByteString -> Builder
quote q xs = either (fmtErrorBs q xs) (inQuotes . byteString)

buildAction :: Connection        -- ^ Connection for string escaping
            -> Query             -- ^ Query for message error
            -> [Action]          -- ^ List of parameters for message error
            -> Action            -- ^ Action to build
            -> IO Builder
buildAction _ _ _     (Plain  b)            = pure b
buildAction conn q xs (Escape s)            = quote q xs <$> escapeStringConn conn s
buildAction conn q xs (EscapeByteA s)       = quote q xs <$> escapeByteaConn conn s
buildAction conn q xs (EscapeIdentifier s) =
    either (fmtErrorBs q xs) byteString <$> escapeIdentifier conn s
buildAction conn q xs (Many  ys)           =
    mconcat <$> mapM (buildAction conn q xs) ys

checkError :: PQ.Connection -> Maybe a -> IO (Either ByteString a)
checkError _ (Just x) = return $ Right x
checkError c Nothing  = Left . maybe "" id <$> PQ.errorMessage c

escapeWrap       :: (PQ.Connection -> ByteString -> IO (Maybe ByteString))
                 -> Connection
                 -> ByteString
                 -> IO (Either ByteString ByteString)
escapeWrap f conn s =
    withConnection conn $ \c ->
    f c s >>= checkError c

escapeStringConn :: Connection -> ByteString -> IO (Either ByteString ByteString)
escapeStringConn = escapeWrap PQ.escapeStringConn

escapeIdentifier :: Connection -> ByteString -> IO (Either ByteString ByteString)
escapeIdentifier = escapeWrap PQ.escapeIdentifier

escapeByteaConn :: Connection -> ByteString -> IO (Either ByteString ByteString)
escapeByteaConn = escapeWrap PQ.escapeByteaConn

breakOnSingleQuestionMark :: ByteString -> (ByteString, ByteString)
breakOnSingleQuestionMark b = go (B8.empty, b)
  where go (x,bs) = (x `B8.append` x',bs')
                -- seperate from first QM
          where tup@(noQ, restWithQ) = B8.break (=='?') bs
                -- if end of query, just return
                -- else check for second QM in 'go2'
                (x', bs') = maybe tup go2 $
                    -- drop found QM and peek at next char
                    B8.uncons restWithQ >>= B8.uncons . snd
                -- another QM after the first means:
                -- take literal QM and keep going.
                go2 ('?', t2) = go (noQ `B8.snoc` '?',t2)
                -- Anything else means
                go2 _ = tup
