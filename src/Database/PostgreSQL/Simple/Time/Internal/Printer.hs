{-# LANGUAGE BangPatterns, ViewPatterns #-}

------------------------------------------------------------------------------
-- Module:      Database.PostgreSQL.Simple.Time.Internal.Printer
-- Copyright:   (c) 2012-2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Time.Internal.Printer
    (
      day
    , timeOfDay
    , timeZone
    , utcTime
    , localTime
    , zonedTime
    , nominalDiffTime
    ) where

import Control.Arrow ((>>>))
import Data.ByteString.Builder (Builder, integerDec)
import Data.ByteString.Builder.Prim
    ( liftFixedToBounded, (>$<), (>*<)
    , BoundedPrim, primBounded, condB, emptyB, FixedPrim, char8, int32Dec)
import Data.Char ( chr )
import Data.Int ( Int32, Int64 )
import Data.Time
    ( UTCTime(..), ZonedTime(..), LocalTime(..), NominalDiffTime
    , Day, toGregorian, TimeOfDay(..), timeToTimeOfDay
    , TimeZone, timeZoneMinutes )
import Database.PostgreSQL.Simple.Compat ((<>), fromPico)
import Unsafe.Coerce (unsafeCoerce)

liftB :: FixedPrim a -> BoundedPrim a
liftB = liftFixedToBounded

digit   :: FixedPrim Int
digit   = (\x -> chr (x + 48)) >$< char8

digits2 :: FixedPrim Int
digits2 = (`quotRem` 10) >$< (digit >*< digit)

digits3 :: FixedPrim Int
digits3 = (`quotRem` 10) >$< (digits2 >*< digit)

digits4 :: FixedPrim Int
digits4 = (`quotRem` 10) >$< (digits3 >*< digit)

frac :: BoundedPrim Int64
frac = condB (== 0) emptyB ((,) '.' >$< (liftB char8 >*< trunc12))
  where
    trunc12 :: BoundedPrim Int64
    trunc12 = (`quotRem` 1000000) >$<
              condB (\(_,y) -> y == 0)
                    (fst >$< trunc6)
                    (liftB digits6 >*< trunc6)

    digitB  = liftB digit

    digits6 = (fromIntegral >>> (`quotRem` 10)) >$< (digits5 >*< digit)
    digits5 =                   (`quotRem` 10)  >$< (digits4 >*< digit)

    trunc6 =    (fromIntegral >>> (`quotRem` 100000)) >$< (digitB >*< trunc5)
    trunc5 = condB (== 0) emptyB ((`quotRem`  10000)  >$< (digitB >*< trunc4))
    trunc4 = condB (== 0) emptyB ((`quotRem`   1000)  >$< (digitB >*< trunc3))
    trunc3 = condB (== 0) emptyB ((`quotRem`    100)  >$< (digitB >*< trunc2))
    trunc2 = condB (== 0) emptyB ((`quotRem`     10)  >$< (digitB >*< trunc1))
    trunc1 = condB (== 0) emptyB digitB


year  :: BoundedPrim Int32
year = condB (>= 10000) int32Dec (checkBCE >$< liftB digits4)
  where
    checkBCE :: Int32 -> Int
    checkBCE y
        | y > 0     = fromIntegral y
        | otherwise = error msg

    msg = "Database.PostgreSQL.Simple.Time.Printer.year:  years BCE not supported"

day :: BoundedPrim Day
day = toYMD >$< (year >*< liftB (char8 >*< digits2 >*< char8 >*< digits2))
  where
    toYMD (toGregorian -> (fromIntegral -> !y, !m,!d)) = (y,('-',(m,('-',d))))

timeOfDay :: BoundedPrim TimeOfDay
timeOfDay = f >$< (hh_mm_ >*< ss)
  where
    f (TimeOfDay h m s)  =  ((h,(':',(m,':'))),s)

    hh_mm_ = liftB (digits2 >*< char8 >*< digits2 >*< char8)

    ss = (\s -> fromIntegral (fromPico s) `quotRem` 1000000000000) >$<
         (liftB (fromIntegral >$< digits2) >*< frac)

timeZone :: BoundedPrim TimeZone
timeZone = timeZoneMinutes >$< tz
  where
    tz  = condB (>= 0) ((,) '+' >$< tzh) ((,) '-' . negate >$< tzh)

    tzh = liftB char8 >*< ((`quotRem` 60) >$< (liftB digits2 >*< tzm))

    tzm = condB (==0) emptyB ((,) ':' >$< liftB (char8 >*< digits2))

utcTime :: BoundedPrim UTCTime
utcTime = f >$< (day >*< liftB char8 >*< timeOfDay >*< liftB char8)
  where f (UTCTime d (timeToTimeOfDay -> tod)) = (d,(' ',(tod,'Z')))

localTime :: BoundedPrim LocalTime
localTime = f >$< (day >*< liftB char8 >*< timeOfDay)
  where f (LocalTime d tod) = (d, (' ', tod))

zonedTime :: BoundedPrim ZonedTime
zonedTime = f >$< (localTime >*< timeZone)
  where f (ZonedTime lt tz) = (lt, tz)


nominalDiffTime :: NominalDiffTime -> Builder
nominalDiffTime xy = integerDec x <> primBounded frac (abs (fromIntegral y))
  where
    (x,y) = fromPico (unsafeCoerce xy) `quotRem` 1000000000000
