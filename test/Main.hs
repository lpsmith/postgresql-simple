{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Exception            (bracket)
import Control.Monad                (when)
import Database.PostgreSQL.Simple
import System.Exit                  (exitFailure)
import System.IO
import Test.HUnit

import Bytea
import Notify

-- | Action for connecting to the database that will be used for testing.
--
-- Note that some tests, such as Notify, use multiple connections, and assume
-- that 'testConnect' connects to the same database every time it is called.
testConnect :: IO Connection
testConnect = connectPostgreSQL ""

withConn :: (Connection -> IO a) -> IO a
withConn = bracket testConnect close

tests :: Connection -> [Test]
tests conn =
    [ TestLabel "Bytea"     $ testBytea     conn
    , TestLabel "Notify"    $ testNotify    conn    withConn
    ]

main :: IO ()
main = do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    Counts{cases, tried, errors, failures} <-
        withConn $ runTestTT . TestList . tests
    when (cases /= tried || errors /= 0 || failures /= 0) $ exitFailure
