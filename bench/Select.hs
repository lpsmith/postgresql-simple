{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.Vector as V
import qualified Database.PostgreSQL.Simple.Vector.Unboxed as VU

import           System.Environment (getArgs)
import           Data.Foldable (Foldable, foldl')
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
    args <- getArgs
    conn <- connectPostgreSQL ""
    case args of
        ("vector":_) -> do
            result <- V.query_ conn "SELECT * FROM generate_series(1, 10000000);"
            print (process result)
        ("unboxed":_) -> do
            -- dummy column
            result <- VU.query_ conn "SELECT (NULL :: VOID), * FROM generate_series(1, 10000000);"
            print (process' result)
        _ -> do
            result <- query_ conn "SELECT * FROM generate_series(1, 10000000);"
            print (process result)

process :: Foldable f => f (Only Int) ->  Int
process = foldl' (\x (Only y) -> max x y) 0

process' :: VU.Vector ((), Int) ->  Int
process' = VU.foldl' (\x (_, y) -> max x y) 0
