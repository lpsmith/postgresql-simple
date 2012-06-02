------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time.Implementation
-- Copyright:   (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, PatternGuards #-}

module Database.PostgreSQL.Simple.Time.Implementation where

import Prelude hiding (take)
--import Blaze.ByteString.Builder(fromByteString)
--import Blaze.ByteString.Builder.Char8(fromString, fromChar)
import Control.Arrow((***))
import Control.Applicative
import Control.Exception
import Control.Monad(when)
import Data.Bits((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w)
import Data.Char
import Data.Time hiding (getTimeZone, getZonedTime)
import Data.Typeable
import Data.Word(Word8)
import qualified Data.Attoparsec.Char8 as A
import qualified Database.PostgreSQL.Simple.BuiltinTypes as PG
import Database.PostgreSQL.Simple.FromField
--import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Ok

data Unbounded a
   = NegInfinity
   | Finite !a
   | PosInfinity
     deriving (Eq, Ord, Typeable)

instance Show a => Show (Unbounded a) where
  showsPrec prec x rest
    = case x of
        NegInfinity -> "-infinity" ++ rest
        Finite time -> showsPrec prec time rest
        PosInfinity ->  "infinity" ++ rest

instance Read a => Read (Unbounded a) where
  readsPrec prec = readParen False $ \str -> case str of
    ('-':'i':'n':'f':'i':'n':'i':'t':'y':xs)  -> [(NegInfinity,xs)]
    (    'i':'n':'f':'i':'n':'i':'t':'y':xs)  -> [(PosInfinity,xs)]
    xs -> map (Finite *** id) (readsPrec prec xs)

type LocalTimestamp = Unbounded LocalTime
type UTCTimestamp   = Unbounded UTCTime
type ZonedTimestamp = Unbounded ZonedTime
type Date           = Unbounded Day

instance FromField UTCTimestamp where
  fromField = ff PG.TimestampWithTimeZone "UTCTimestamp" parseUTCTimestamp

instance FromField ZonedTimestamp where
  fromField = ff PG.TimestampWithTimeZone "ZonedTimestamp" parseZonedTimestamp

instance FromField LocalTimestamp where
  fromField = ff PG.Timestamp "LocalTimestamp" parseLocalTimestamp

instance FromField Date where
  fromField = ff PG.Date "Date" parseDate

ff :: PG.BuiltinType -> String -> (B.ByteString -> Either String a)
   -> Field -> Maybe B.ByteString -> Ok a
ff pgType hsType parse f mstr
    | typeOid f /= PG.builtin2oid pgType
    = left (Incompatible   (B8.unpack (typename f)) hsType "")
    | Nothing <- mstr 
    = left (UnexpectedNull (B8.unpack (typename f)) hsType "")
    | Just str <- mstr 
    = case parse str of 
        Left msg -> left (ConversionFailed (B8.unpack (typename f)) hsType msg)
        Right val -> pure val
{-# INLINE ff #-}

left :: Exception err => err -> Ok a
left err = Errors [SomeException err]
{-# INLINE left #-}

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
getDay = do
    yearStr <- A.takeWhile isDigit
    when (B.length yearStr < 4) (fail "year must consist of at least 4 digits")

    let !year = toNum yearStr
    _       <- A.char '-'
    month   <- digits "month"
    _       <- A.char '-'
    day     <- digits "day"

    case fromGregorianValid year month day of
      Nothing -> fail "invalid date"
      Just x  -> return $! x

getDate :: A.Parser Date
getDate = getUnbounded getDay

toPicos :: Num a => B.ByteString -> a
toPicos str = toNum str * 10^(12 - B.length str)

getTimeOfDay :: A.Parser TimeOfDay
getTimeOfDay = do
    hour   <- digits "hours"
    _      <- A.char ':'
    minute <- digits "minutes"
    _      <- A.char ':'
    second <- digits "seconds"
    picos  <- (A.char '.' *> (toPicos <$> A.takeWhile1 isDigit)) <|> return 0

    let !picos' = second * 1000000000000 + picos

    case makeTimeOfDayValid hour minute picos' of
      Nothing -> fail "invalid time of day"
      Just x  -> return $! x

getLocalTime :: A.Parser LocalTime
getLocalTime = LocalTime <$> getDay <*> (A.char ' ' *> getTimeOfDay)

getLocalTimestamp :: A.Parser LocalTimestamp
getLocalTimestamp = getUnbounded getLocalTime

getTimeZone :: A.Parser TimeZone
getTimeZone = do
    sign   <- A.satisfy (\c -> c == '+' || c == '-')
    offset <- digits "timezone"
    let !minutes = 60 * if sign == '+' then offset else -offset
    return $! minutesToTimeZone minutes

getZonedTime :: A.Parser ZonedTime
getZonedTime = ZonedTime <$> getLocalTime <*> getTimeZone

getZonedTimestamp :: A.Parser ZonedTimestamp
getZonedTimestamp = getUnbounded getZonedTime

getUTCTime :: A.Parser UTCTime
getUTCTime = do
    day  <- getDay
    _    <- A.char ' '
    time <- getTimeOfDay
    zone <- getTimeZone
    let (!dayDelta,!time') = utcToLocalTimeOfDay zone time
    let !day' = addDays dayDelta day
    let !time'' = timeOfDayToTime time'
    return (UTCTime day' time'')

getUTCTimestamp :: A.Parser UTCTimestamp
getUTCTimestamp = getUnbounded getUTCTime

toNum :: Num n => B.ByteString -> n
toNum = B.foldl' (\a c -> 10*a + digit c) 0
{-# INLINE toNum #-}

digit :: Num n => Word8 -> n
digit c = fromIntegral (c .&. 0x0f)
{-# INLINE digit #-}

digits :: Num n => String -> A.Parser n
digits msg = do
  x <- A.anyChar
  y <- A.anyChar
  if isDigit x && isDigit y
  then return $! (10 * digit (c2w x) + digit (c2w y))
  else fail (msg ++ " is not 2 digits")
{-# INLINE digits #-}
