module Database.PostgreSQL.Simple.Time.Interval where

import Data.Int

data Interval = Interval { intervalMonths :: Int32
                         , intervalDays :: Int32
                         , intervalMicroseconds :: Integer }
                         deriving (Show, Read, Eq)

zeroInterval :: Interval
zeroInterval = Interval 0 0 0