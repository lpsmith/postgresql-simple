------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time.Implementation
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Database.PostgreSQL.Simple.Time.Implementation where

import Prelude hiding (take)
import Data.ByteString.Builder(Builder, byteString)
import Data.ByteString.Builder.Prim(primBounded)
import Control.Arrow((***))
import Control.Applicative
import qualified Data.ByteString as B
import Data.Time hiding (getTimeZone, getZonedTime)
import Data.Typeable
import Data.Maybe (fromMaybe)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Database.PostgreSQL.Simple.Compat ((<>))
import qualified Database.PostgreSQL.Simple.Time.Internal.Parser  as TP
import qualified Database.PostgreSQL.Simple.Time.Internal.Printer as TPP

data Unbounded a
   = NegInfinity
   | Finite !a
   | PosInfinity
     deriving (Eq, Ord, Typeable, Functor)

instance Show a => Show (Unbounded a) where
  showsPrec prec x rest
    = case x of
        NegInfinity -> "-infinity" <> rest
        Finite time -> showsPrec prec time rest
        PosInfinity ->  "infinity" <> rest

instance Read a => Read (Unbounded a) where
  readsPrec prec = readParen False $ \str -> case str of
    ('-':'i':'n':'f':'i':'n':'i':'t':'y':xs)  -> [(NegInfinity,xs)]
    (    'i':'n':'f':'i':'n':'i':'t':'y':xs)  -> [(PosInfinity,xs)]
    xs -> map (Finite *** id) (readsPrec prec xs)

type LocalTimestamp = Unbounded LocalTime
type UTCTimestamp   = Unbounded UTCTime
type ZonedTimestamp = Unbounded ZonedTime
type Date           = Unbounded Day

parseUTCTime   :: B.ByteString -> Either String UTCTime
parseUTCTime   = A.parseOnly (getUTCTime <* A.endOfInput)

parseZonedTime :: B.ByteString -> Either String ZonedTime
parseZonedTime = A.parseOnly (getZonedTime <* A.endOfInput)

parseLocalTime :: B.ByteString -> Either String LocalTime
parseLocalTime = A.parseOnly (getLocalTime <* A.endOfInput)

parseDay :: B.ByteString -> Either String Day
parseDay = A.parseOnly (getDay <* A.endOfInput)

parseTimeOfDay :: B.ByteString -> Either String TimeOfDay
parseTimeOfDay = A.parseOnly (getTimeOfDay <* A.endOfInput)

parseUTCTimestamp   :: B.ByteString -> Either String UTCTimestamp
parseUTCTimestamp   = A.parseOnly (getUTCTimestamp <* A.endOfInput)

parseZonedTimestamp :: B.ByteString -> Either String ZonedTimestamp
parseZonedTimestamp = A.parseOnly (getZonedTimestamp <* A.endOfInput)

parseLocalTimestamp :: B.ByteString -> Either String LocalTimestamp
parseLocalTimestamp = A.parseOnly (getLocalTimestamp <* A.endOfInput)

parseDate :: B.ByteString -> Either String Date
parseDate = A.parseOnly (getDate <* A.endOfInput)

getUnbounded :: A.Parser a -> A.Parser (Unbounded a)
getUnbounded getFinite
    =     (pure NegInfinity <* A.string "-infinity")
      <|> (pure PosInfinity <* A.string  "infinity")
      <|> (Finite <$> getFinite)

getDay :: A.Parser Day
getDay = TP.day

getDate :: A.Parser Date
getDate = getUnbounded getDay

getTimeOfDay :: A.Parser TimeOfDay
getTimeOfDay = TP.timeOfDay

getLocalTime :: A.Parser LocalTime
getLocalTime = TP.localTime

getLocalTimestamp :: A.Parser LocalTimestamp
getLocalTimestamp = getUnbounded getLocalTime

getTimeZone :: A.Parser TimeZone
getTimeZone = fromMaybe utc <$> TP.timeZone

type TimeZoneHMS = (Int,Int,Int)

getTimeZoneHMS :: A.Parser TimeZoneHMS
getTimeZoneHMS = munge <$> TP.timeZoneHMS
  where
    munge Nothing = (0,0,0)
    munge (Just (TP.UTCOffsetHMS h m s)) = (h,m,s)

localToUTCTimeOfDayHMS :: TimeZoneHMS -> TimeOfDay -> (Integer, TimeOfDay)
localToUTCTimeOfDayHMS (dh, dm, ds) tod =
    TP.localToUTCTimeOfDayHMS (TP.UTCOffsetHMS dh dm ds) tod

getZonedTime :: A.Parser ZonedTime
getZonedTime = TP.zonedTime

getZonedTimestamp :: A.Parser ZonedTimestamp
getZonedTimestamp = getUnbounded getZonedTime

getUTCTime :: A.Parser UTCTime
getUTCTime = TP.utcTime

getUTCTimestamp :: A.Parser UTCTimestamp
getUTCTimestamp = getUnbounded getUTCTime

dayToBuilder :: Day -> Builder
dayToBuilder = primBounded TPP.day

timeOfDayToBuilder :: TimeOfDay -> Builder
timeOfDayToBuilder = primBounded TPP.timeOfDay

timeZoneToBuilder :: TimeZone -> Builder
timeZoneToBuilder = primBounded TPP.timeZone

utcTimeToBuilder :: UTCTime -> Builder
utcTimeToBuilder = primBounded TPP.utcTime

zonedTimeToBuilder :: ZonedTime -> Builder
zonedTimeToBuilder = primBounded TPP.zonedTime

localTimeToBuilder :: LocalTime -> Builder
localTimeToBuilder = primBounded TPP.localTime

unboundedToBuilder :: (a -> Builder) -> (Unbounded a -> Builder)
unboundedToBuilder finiteToBuilder unbounded
    = case unbounded of
        NegInfinity -> byteString "-infinity"
        Finite a    -> finiteToBuilder a
        PosInfinity -> byteString  "infinity"

utcTimestampToBuilder :: UTCTimestamp -> Builder
utcTimestampToBuilder = unboundedToBuilder utcTimeToBuilder

zonedTimestampToBuilder :: ZonedTimestamp -> Builder
zonedTimestampToBuilder = unboundedToBuilder zonedTimeToBuilder

localTimestampToBuilder :: LocalTimestamp -> Builder
localTimestampToBuilder = unboundedToBuilder localTimeToBuilder

dateToBuilder  :: Date -> Builder
dateToBuilder  = unboundedToBuilder dayToBuilder

nominalDiffTimeToBuilder :: NominalDiffTime -> Builder
nominalDiffTimeToBuilder = TPP.nominalDiffTime
