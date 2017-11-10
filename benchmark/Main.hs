{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Database.PostgreSQL.Simple
import qualified Data.Map as Map
import Data.Text
import Data.Time
import Data.Aeson as Aeson
import Database.PostgreSQL.Simple.HStore
import Control.Exception
import Control.DeepSeq

instance NFData HStoreMap where
  rnf (HStoreMap m) = rnf m

instance NFData SomeException where
  rnf e = rnf (show e)

main :: IO ()
main = do
  t <- getCurrentTime
  bracket (connectPostgreSQL "host=localhost port=5432 user=opaleye password=opaleye") close $ \conn -> do 
    defaultMain
      [ bgroup "complicatedQuery"
        [ bench "without error" $ nfIO $ complicatedQuery conn
        , bench "with error" $ nfIO $ catch (complicatedQueryWithError conn) (\ (e :: SomeException) -> e `deepseq` pure [(t, HStoreMap mempty, Aeson.String "")])
        ]
      ]

complicatedQuery :: Connection -> IO [(UTCTime, Value, HStoreMap)]
complicatedQuery conn = do
  let j = Map.fromList [("fo\"o","bar"),("b\\ar","baz"),("baz","\"value\\with\"escapes")] :: Map.Map Text Text
  query conn "SELECT current_timestamp, ?::json, ?::hstore" (toJSON j, HStoreMap j)

complicatedQueryWithError :: Connection -> IO [(UTCTime, HStoreMap, Value)]
complicatedQueryWithError conn = do
  let j = Map.fromList [("fo\"o","bar"),("b\\ar","baz"),("baz","\"value\\with\"escapes")] :: Map.Map Text Text
  query conn "SELECT current_timestamp, ?::json, ?::hstore" (toJSON j, HStoreMap j)
