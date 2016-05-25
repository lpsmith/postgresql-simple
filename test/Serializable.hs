module Serializable (testSerializable) where

import Common
import Control.Concurrent
import Control.Exception as E
import Data.IORef
import Database.PostgreSQL.Simple.Transaction

initCounter :: Connection -> IO ()
initCounter conn = do
    0 <- execute_ conn "DROP TABLE IF EXISTS testSerializableCounter;\
                      \ CREATE TABLE testSerializableCounter (n INT)"
    1 <- execute_ conn "INSERT INTO testSerializableCounter VALUES (0)"
    return ()

getCounter :: Connection -> IO Int
getCounter conn = do
    [Only n] <- query_ conn "SELECT n FROM testSerializableCounter"
    return n

putCounter :: Connection -> Int -> IO ()
putCounter conn n = do
    1 <- execute conn "UPDATE testSerializableCounter SET n=?" (Only n)
    return ()

testSerializable :: TestEnv -> Assertion
testSerializable TestEnv{..} =
    withConn $ \conn2 -> do
        initCounter conn

        attemptCounter  <- newIORef (0 :: Int)
        readyToBother   <- newEmptyMVar
        bothered        <- newEmptyMVar
        finished        <- newEmptyMVar

        _ <- forkIO $ do
            withTransactionSerializable conn2 $ do
                modifyIORef attemptCounter (+1)
                n <- getCounter conn2
                True <- tryPutMVar readyToBother ()
                readMVar bothered
                putCounter conn2 (n+1)
            putMVar finished ()

        takeMVar readyToBother
        withTransactionSerializable conn $ do
            n <- getCounter conn
            putCounter conn (n+1)
        True <- tryPutMVar bothered ()

        takeMVar finished

        ac <- readIORef attemptCounter
        assertEqual "attemptCounter" 2 ac

        ok <- E.catch (do withTransactionSerializable conn (fail "Whoops")
                          return False)
                      (\(_ :: IOException) -> return True)
        assertBool "Exceptions (besides serialization failure) should be\
                   \ propagated through withTransactionSerializable"
                   ok

        -- Make sure transaction isn't dangling
        1 <- execute_ conn "UPDATE testSerializableCounter SET n=12345"
        0 <- execute_ conn "ROLLBACK"
            -- This prints "NOTICE:  there is no transaction in progress"
        [Only (12345 :: Int)] <- query_ conn "SELECT n FROM testSerializableCounter"
        return ()
