module Fold (testFold) where

import Common

testFold :: TestEnv -> Test
testFold TestEnv{..} = TestCase $ do
    xs <- fold_ conn "SELECT generate_series(1,10000)"
            [] $ \xs (Only x) -> return (x:xs)
    True <- return $ reverse xs == ([1..10000] :: [Int])

    -- TODO: add more complete tests, e.g.:
    --
    --  * Fold in a transaction
    --
    --  * Fold in a transaction after a previous fold has been performed
    --
    --  * Nested fold

    return ()
