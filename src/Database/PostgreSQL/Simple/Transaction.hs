{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Transaction
-- Copyright:   (c) 2011-2013 Leon P Smith
--              (c) 2013 Joey Adams
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Transaction
    (
    -- * Transaction handling
      withTransaction
    , withTransactionLevel
    , withTransactionMode
    , withTransactionModeRetry
    , withTransactionSerializable
    , TransactionMode(..)
    , IsolationLevel(..)
    , ReadWriteMode(..)
    , defaultTransactionMode
    , defaultIsolationLevel
    , defaultReadWriteMode
--    , Base.autocommit
    , begin
    , beginLevel
    , beginMode
    , commit
    , rollback

    -- * Savepoint
    , withSavepoint
    , Savepoint
    , newSavepoint
    , releaseSavepoint
    , rollbackToSavepoint
    , rollbackToAndReleaseSavepoint

    -- * Error predicates
    , isSerializationError
    , isNoActiveTransactionError
    , isFailedTransactionError
    ) where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.Compat (mask, (<>))
import GHC.Stack


-- | Of the four isolation levels defined by the SQL standard,
-- these are the three levels distinguished by PostgreSQL as of version 9.0.
-- See <https://www.postgresql.org/docs/9.5/static/transaction-iso.html>
-- for more information.   Note that prior to PostgreSQL 9.0, 'RepeatableRead'
-- was equivalent to 'Serializable'.

data IsolationLevel
   = DefaultIsolationLevel  -- ^ the isolation level will be taken from
                            --   PostgreSQL's per-connection
                            --   @default_transaction_isolation@ variable,
                            --   which is initialized according to the
                            --   server's config.  The default configuration
                            --   is 'ReadCommitted'.
   | ReadCommitted
   | RepeatableRead
   | Serializable
     deriving (Show, Eq, Ord, Enum, Bounded)

data ReadWriteMode
   = DefaultReadWriteMode   -- ^ the read-write mode will be taken from
                            --   PostgreSQL's per-connection
                            --   @default_transaction_read_only@ variable,
                            --   which is initialized according to the
                            --   server's config.  The default configuration
                            --   is 'ReadWrite'.
   | ReadWrite
   | ReadOnly
     deriving (Show, Eq, Ord, Enum, Bounded)

data TransactionMode = TransactionMode {
       isolationLevel :: !IsolationLevel,
       readWriteMode  :: !ReadWriteMode
     } deriving (Show, Eq)

defaultTransactionMode :: (HasCallStack) => TransactionMode
defaultTransactionMode =  TransactionMode
                            defaultIsolationLevel
                            defaultReadWriteMode

defaultIsolationLevel  :: (HasCallStack) => IsolationLevel
defaultIsolationLevel  =  DefaultIsolationLevel

defaultReadWriteMode   :: (HasCallStack) => ReadWriteMode
defaultReadWriteMode   =  DefaultReadWriteMode

-- | Execute an action inside a SQL transaction.
--
-- This function initiates a transaction with a \"@begin
-- transaction@\" statement, then executes the supplied action.  If
-- the action succeeds, the transaction will be completed with
-- 'Base.commit' before this function returns.
--
-- If the action throws /any/ kind of exception (not just a
-- PostgreSQL-related exception), the transaction will be rolled back using
-- 'rollback', then the exception will be rethrown.
--
-- For nesting transactions, see 'withSavepoint'.
withTransaction :: (HasCallStack) => Connection -> IO a -> IO a
withTransaction = withTransactionMode defaultTransactionMode

-- | Execute an action inside of a 'Serializable' transaction.  If a
-- serialization failure occurs, roll back the transaction and try again.
-- Be warned that this may execute the IO action multiple times.
--
-- A 'Serializable' transaction creates the illusion that your program has
-- exclusive access to the database.  This means that, even in a concurrent
-- setting, you can perform queries in sequence without having to worry about
-- what might happen between one statement and the next.
--
-- Think of it as STM, but without @retry@.
withTransactionSerializable :: (HasCallStack) => Connection -> IO a -> IO a
withTransactionSerializable =
    withTransactionModeRetry
        TransactionMode
        { isolationLevel = Serializable
        , readWriteMode  = ReadWrite
        }
        isSerializationError

-- | Execute an action inside a SQL transaction with a given isolation level.
withTransactionLevel :: (HasCallStack) => IsolationLevel -> Connection -> IO a -> IO a
withTransactionLevel lvl
    = withTransactionMode defaultTransactionMode { isolationLevel = lvl }

-- | Execute an action inside a SQL transaction with a given transaction mode.
withTransactionMode :: (HasCallStack) => TransactionMode -> Connection -> IO a -> IO a
withTransactionMode mode conn act =
  mask $ \restore -> do
    beginMode mode conn
    r <- restore act `E.onException` rollback_ conn
    commit conn
    return r

-- | Like 'withTransactionMode', but also takes a custom callback to
-- determine if a transaction should be retried if an 'SqlError' occurs.
-- If the callback returns True, then the transaction will be retried.
-- If the callback returns False, or an exception other than an 'SqlError'
-- occurs then the transaction will be rolled back and the exception rethrown.
--
-- This is used to implement 'withTransactionSerializable'.
withTransactionModeRetry :: (HasCallStack) => TransactionMode -> (SqlError -> Bool) -> Connection -> IO a -> IO a
withTransactionModeRetry mode shouldRetry conn act =
    mask $ \restore ->
        retryLoop $ E.try $ do
            a <- restore act
            commit conn
            return a
  where
    retryLoop :: (HasCallStack) => IO (Either E.SomeException a) -> IO a
    retryLoop act' = do
        beginMode mode conn
        r <- act'
        case r of
            Left e -> do
                rollback_ conn
                case fmap shouldRetry (E.fromException e) of
                  Just True -> retryLoop act'
                  _ -> E.throwIO e
            Right a ->
                return a

-- | Rollback a transaction.
rollback :: (HasCallStack) => Connection -> IO ()
rollback conn = execute_ conn "ABORT" >> return ()

-- | Rollback a transaction, ignoring any @IOErrors@
rollback_ :: (HasCallStack) => Connection -> IO ()
rollback_ conn = rollback conn `E.catch` \(_ :: IOError) -> return ()

-- | Commit a transaction.
commit :: (HasCallStack) => Connection -> IO ()
commit conn = execute_ conn "COMMIT" >> return ()

-- | Begin a transaction.
begin :: (HasCallStack) => Connection -> IO ()
begin = beginMode defaultTransactionMode

-- | Begin a transaction with a given isolation level
beginLevel :: (HasCallStack) => IsolationLevel -> Connection -> IO ()
beginLevel lvl = beginMode defaultTransactionMode { isolationLevel = lvl }

-- | Begin a transaction with a given transaction mode
beginMode :: (HasCallStack) => TransactionMode -> Connection -> IO ()
beginMode mode conn = do
    _ <- execute_ conn $! Query (B.concat ["BEGIN", isolevel, readmode])
    return ()
  where
    isolevel = case isolationLevel mode of
                 DefaultIsolationLevel -> ""
                 ReadCommitted  -> " ISOLATION LEVEL READ COMMITTED"
                 RepeatableRead -> " ISOLATION LEVEL REPEATABLE READ"
                 Serializable   -> " ISOLATION LEVEL SERIALIZABLE"
    readmode = case readWriteMode mode of
                 DefaultReadWriteMode -> ""
                 ReadWrite -> " READ WRITE"
                 ReadOnly  -> " READ ONLY"

------------------------------------------------------------------------
-- Savepoint

-- | Create a savepoint, and roll back to it if an error occurs.  This may only
-- be used inside of a transaction, and provides a sort of
-- \"nested transaction\".
--
-- See <https://www.postgresql.org/docs/9.5/static/sql-savepoint.html>
withSavepoint :: (HasCallStack) => Connection -> IO a -> IO a
withSavepoint conn body =
  mask $ \restore -> do
    sp <- newSavepoint conn
    r <- restore body `E.onException` rollbackToAndReleaseSavepoint conn sp
    releaseSavepoint conn sp `E.catch` \err ->
        if isFailedTransactionError err
            then rollbackToAndReleaseSavepoint conn sp
            else E.throwIO err
    return r

-- | Create a new savepoint.  This may only be used inside of a transaction.
newSavepoint :: (HasCallStack) => Connection -> IO Savepoint
newSavepoint conn = do
    name <- newTempName conn
    _ <- execute_ conn ("SAVEPOINT " <> name)
    return (Savepoint name)

-- | Destroy a savepoint, but retain its effects.
--
-- Warning: this will throw a 'SqlError' matching 'isFailedTransactionError' if
-- the transaction is aborted due to an error.  'commit' would merely warn and
-- roll back.
releaseSavepoint :: (HasCallStack) => Connection -> Savepoint -> IO ()
releaseSavepoint conn (Savepoint name) =
    execute_ conn ("RELEASE SAVEPOINT " <> name) >> return ()

-- | Roll back to a savepoint.  This will not release the savepoint.
rollbackToSavepoint :: (HasCallStack) => Connection -> Savepoint -> IO ()
rollbackToSavepoint conn (Savepoint name) =
    execute_ conn ("ROLLBACK TO SAVEPOINT " <> name) >> return ()

-- | Roll back to a savepoint and release it.  This is like calling
-- 'rollbackToSavepoint' followed by 'releaseSavepoint', but avoids a
-- round trip to the database server.
rollbackToAndReleaseSavepoint :: (HasCallStack) => Connection -> Savepoint -> IO ()
rollbackToAndReleaseSavepoint conn (Savepoint name) =
    execute_ conn sql >> return ()
  where
    sql = "ROLLBACK TO SAVEPOINT " <> name <> "; RELEASE SAVEPOINT " <> name
