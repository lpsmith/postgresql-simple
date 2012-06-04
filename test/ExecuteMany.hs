module ExecuteMany (testExecuteMany) where

import Common

import Data.ByteString (ByteString)

testExecuteMany :: TestEnv -> Test
testExecuteMany TestEnv{..} = TestCase $ do
    execute_ conn "CREATE TEMPORARY TABLE tmp_executeMany (i INT, t TEXT, b BYTEA)"

    let rows :: [(Int, String, Binary ByteString)]
        rows = [ (1, "hello", Binary "bye")
               , (2, "world", Binary "\0\r\t\n")
               , (3, "?",     Binary "")
               ]

    count <- executeMany conn "INSERT INTO tmp_executeMany VALUES (?, ?, ?)" rows
    assertEqual "count" (fromIntegral $ length rows) count

    rows' <- query_ conn "SELECT * FROM tmp_executeMany"
    assertEqual "rows" rows rows'

    return ()
