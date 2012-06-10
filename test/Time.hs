{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

{-

Testing strategies:

fromString . toString == id           ** Todo?

toString . fromString == almost id    ** Todo?

postgresql -> haskell -> postgresql   *  Done

haskell -> postgresql -> haskell      ** Todo?

But still,  what we really want to establish is that the two values
correspond;  for example,  a conversion that consistently added hour
when printed to a string and subtracted an hour when parsed from string
would still pass these tests.

-}

module Time (testTime) where

import Common
import Control.Monad(forM_, replicateM_)
import Data.Time
import Data.ByteString(ByteString)
import Database.PostgreSQL.Simple.SqlQQ

numTests :: Int
numTests = 200

testTime :: TestEnv -> Test
testTime env@TestEnv{..} = TestCase $ do
  initializeTable env
  execute_ conn "SET timezone TO 'UTC'"
  checkRoundTrips env
  execute_ conn "SET timezone TO 'America/Chicago'"
  checkRoundTrips env
  execute_ conn "SET timezone TO 'Asia/Tokyo'"
  checkRoundTrips env

initializeTable :: TestEnv -> IO ()
initializeTable TestEnv{..} = withTransaction conn $ do
  execute_ conn
     [sql| CREATE TEMPORARY TABLE testtime
             ( x serial, y timestamptz, PRIMARY KEY(x) ) |]
  let pop :: ByteString ->  Double -> IO () = \x y ->
               replicateM_ numTests $ execute conn
                 [sql| INSERT INTO testtime (y) VALUES
                         ('1900-01-01 00:00:00+00'::timestamptz
                          + ?::interval * ROUND(RANDOM() * ?)) |] (x,y)
  pop   "1 microsecond"  6.3113904e15
  pop  "10 microseconds" 6.3113904e14
  pop "100 microseconds" 6.3113904e13
  pop   "1 millisecond"  6.3113904e12
  pop  "10 milliseconds" 6.3113904e11
  pop "100 milliseconds" 6.3113904e10
  pop   "1 second"       6.3113904e9

checkRoundTrips :: TestEnv -> IO ()
checkRoundTrips TestEnv{..} = do
  yxs :: [(UTCTime, Int)] <- query_ conn [sql| SELECT y, x FROM testtime |]
  forM_ yxs $ \yx -> do
      res <- query conn [sql| SELECT y=? FROM testtime WHERE x=? |] yx
      assertBool "UTCTime did not round-trip from SQL to Haskell and back" $
                 res == [Only True]
