{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Common
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.HStore
import qualified Database.PostgreSQL.Simple.Transaction as ST
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Text(Text)
import System.Exit (exitFailure)
import System.IO
import qualified Data.Vector as V

import Notify
import Serializable
import Time

tests :: [TestEnv -> Test]
tests =
    [ TestLabel "Bytea"         . testBytea
    , TestLabel "ExecuteMany"   . testExecuteMany
    , TestLabel "Fold"          . testFold
    , TestLabel "Notify"        . testNotify
    , TestLabel "Serializable"  . testSerializable
    , TestLabel "Time"          . testTime
    , TestLabel "Array"         . testArray
    , TestLabel "HStore"        . testHStore
    , TestLabel "Savepoint"     . testSavepoint
    ]

testBytea :: TestEnv -> Test
testBytea TestEnv{..} = TestList
    [ testStr "empty"                  []
    , testStr "\"hello\""              $ map (fromIntegral . fromEnum) ("hello" :: String)
    , testStr "ascending"              [0..255]
    , testStr "descending"             [255,254..0]
    , testStr "ascending, doubled up"  $ doubleUp [0..255]
    , testStr "descending, doubled up" $ doubleUp [255,254..0]
    ]
  where
    testStr label bytes = TestLabel label $ TestCase $ do
        let bs = B.pack bytes

        [Only h] <- query conn "SELECT md5(?::bytea)" [Binary bs]
        assertBool "Haskell -> SQL conversion altered the string" $ md5 bs == h

        [Only (Binary r)] <- query conn "SELECT ?::bytea" [Binary bs]
        assertBool "SQL -> Haskell conversion altered the string" $ bs == r

    doubleUp = concatMap (\x -> [x, x])

testExecuteMany :: TestEnv -> Test
testExecuteMany TestEnv{..} = TestCase $ do
    execute_ conn "CREATE TEMPORARY TABLE tmp_executeMany (i INT, t TEXT, b BYTEA)"

    let rows :: [(Int, String, Binary ByteString)]
        rows = [ (1, "hello", Binary "bye")
               , (2, "world", Binary "\0\r\t\n")
               , (3, "?",     Binary "")
               ]

    count <- executeMany conn "INSERT INTO tmp_executeMany VALUES (?, ?, ?)" rows
    count @?= fromIntegral (length rows)

    rows' <- query_ conn "SELECT * FROM tmp_executeMany"
    rows' @?= rows

    return ()

testFold :: TestEnv -> Test
testFold TestEnv{..} = TestCase $ do
    xs <- fold_ conn "SELECT generate_series(1,10000)"
            [] $ \xs (Only x) -> return (x:xs)
    reverse xs @?= ([1..10000] :: [Int])

    ref <- newIORef []
    forEach conn "SELECT * FROM generate_series(1,?) a, generate_series(1,?) b"
      (100 :: Int, 50 :: Int) $ \(a :: Int, b :: Int) -> do
        xs <- readIORef ref
        writeIORef ref $! (a,b):xs
    xs <- readIORef ref
    reverse xs @?= [(a,b) | a <- [1..100], b <- [1..50]]

    -- Make sure fold propagates our exception.
    ref <- newIORef []
    True <- expectError (== TestException) $
              forEach_ conn "SELECT generate_series(1,10)" $ \(Only a) ->
                if a == 5 then do
                  -- Cause a SQL error to trip up CLOSE.
                  True <- expectError isSyntaxError $
                          execute_ conn "asdf"
                  True <- expectError ST.isFailedTransactionError $
                          (query_ conn "SELECT 1" :: IO [(Only Int)])
                  throwIO TestException
                else do
                  xs <- readIORef ref
                  writeIORef ref $! (a :: Int) : xs
    xs <- readIORef ref
    reverse xs @?= [1..4]

    withTransaction conn $ replicateM_ 2 $ do
        xs <- fold_ conn "VALUES (1), (2), (3), (4), (5)"
                [] $ \xs (Only x) -> return (x:xs)
        reverse xs @?= ([1..5] :: [Int])

    ref <- newIORef []
    forEach_ conn "SELECT generate_series(1,101)" $ \(Only a) ->
      forEach_ conn "SELECT generate_series(1,55)" $ \(Only b) -> do
        xs <- readIORef ref
        writeIORef ref $! (a :: Int, b :: Int) : xs
    xs <- readIORef ref
    reverse xs @?= [(a,b) | a <- [1..101], b <- [1..55]]

    xs <- fold_ conn "SELECT 1 WHERE FALSE"
            [] $ \xs (Only x) -> return (x:xs)
    xs @?= ([] :: [Int])

    -- TODO: add more complete tests, e.g.:
    --
    --  * Fold in a transaction
    --
    --  * Fold in a transaction after a previous fold has been performed
    --
    --  * Nested fold

    return ()

queryFailure :: forall a. (FromField a, Typeable a, Show a)
             => Connection -> Query -> a -> Assertion
queryFailure conn q resultType = do
  x :: Either SomeException [Only a] <- E.try $ query_ conn q
  case x of
    Left  _   -> return ()
    Right val -> assertFailure ("Did not fail as expected:  "
                              ++ show q
                              ++ " :: "
                              ++ show (typeOf resultType)
                              ++ " -> " ++ show val)

testArray :: TestEnv -> Test
testArray TestEnv{..} = TestCase $ do
    xs <- query_ conn "SELECT '{1,2,3,4}'::_int4"
    xs @?= [Only (V.fromList [1,2,3,4 :: Int])]
    xs <- query_ conn "SELECT '{{1,2},{3,4}}'::_int4"
    xs @?= [Only (V.fromList [V.fromList [1,2],
                              V.fromList [3,4 :: Int]])]
    queryFailure conn "SELECT '{1,2,3,4}'::_int4" (undefined :: V.Vector Bool)
    queryFailure conn "SELECT '{{1,2},{3,4}}'::_int4" (undefined :: V.Vector Int)

testHStore :: TestEnv -> Test
testHStore TestEnv{..} = TestCase $ do
    roundTrip []
    roundTrip [("foo","bar"),("bar","baz"),("baz","hello")]
    roundTrip [("fo\"o","bar"),("b\\ar","baz"),("baz","\"value\\with\"escapes")]
  where
    roundTrip :: [(Text,Text)] -> Assertion
    roundTrip xs = do
      let m = Only (HStoreMap (Map.fromList xs))
      m' <- query conn "SELECT ?::hstore" m
      [m] @?= m'

testSavepoint :: TestEnv -> Test
testSavepoint TestEnv{..} = TestCase $ do
    True <- expectError ST.isNoActiveTransactionError $
            withSavepoint conn $ return ()

    let getRows :: IO [Int]
        getRows = map fromOnly <$> query_ conn "SELECT a FROM tmp_savepoint ORDER BY a"
    withTransaction conn $ do
        execute_ conn "CREATE TEMPORARY TABLE tmp_savepoint (a INT UNIQUE)"
        execute_ conn "INSERT INTO tmp_savepoint VALUES (1)"
        [1] <- getRows

        withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (2)"
            [1,2] <- getRows
            return ()
        [1,2] <- getRows

        withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (3)"
            [1,2,3] <- getRows
            True <- expectError isUniqueViolation $
                execute_ conn "INSERT INTO tmp_savepoint VALUES (2)"
            True <- expectError ST.isFailedTransactionError getRows

            -- Body returning successfully after handling error,
            -- but 'withSavepoint' will roll back without complaining.
            return ()
        -- Rolling back clears the error condition.
        [1,2] <- getRows

        -- 'withSavepoint' will roll back after an exception, even if the
        -- exception wasn't SQL-related.
        True <- expectError (== TestException) $
          withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (3)"
            [1,2,3] <- getRows
            throwIO TestException
        [1,2] <- getRows

        -- Nested savepoint can be rolled back while the
        -- outer effects are retained.
        withSavepoint conn $ do
            execute_ conn "INSERT INTO tmp_savepoint VALUES (3)"
            True <- expectError isUniqueViolation $
              withSavepoint conn $ do
                execute_ conn "INSERT INTO tmp_savepoint VALUES (4)"
                [1,2,3,4] <- getRows
                execute_ conn "INSERT INTO tmp_savepoint VALUES (4)"
            [1,2,3] <- getRows
            return ()
        [1,2,3] <- getRows

        return ()

    -- Transaction committed successfully, even though there were errors
    -- (but we rolled them back).
    [1,2,3] <- getRows

    return ()

data TestException
  = TestException
  deriving (Eq, Show, Typeable)

instance Exception TestException

expectError :: Exception e => (e -> Bool) -> IO a -> IO Bool
expectError p io =
    (io >> return False) `E.catch` \ex ->
    if p ex then return True else throwIO ex

isUniqueViolation :: SqlError -> Bool
isUniqueViolation SqlError{..} = sqlState == "23505"

isSyntaxError :: SqlError -> Bool
isSyntaxError SqlError{..} = sqlState == "42601"

------------------------------------------------------------------------

-- | Action for connecting to the database that will be used for testing.
--
-- Note that some tests, such as Notify, use multiple connections, and assume
-- that 'testConnect' connects to the same database every time it is called.
testConnect :: IO Connection
testConnect = connectPostgreSQL ""

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv cb =
    withConn $ \conn ->
        cb TestEnv
            { conn     = conn
            , withConn = withConn
            }
  where
    withConn = bracket testConnect close

main :: IO ()
main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    Counts{cases, tried, errors, failures} <-
        withTestEnv $ \env -> runTestTT $ TestList $ map ($ env) tests
    when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure
