{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |
-- Module:      Database.PostgreSQL.Simple.Time.Internal.Parser
-- Copyright:   (c) 2012-2015 Leon P Smith
--              (c) 2015 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Parsers for parsing dates and times.

module Database.PostgreSQL.Simple.Time.Internal.Parser
    (
      day
    , localTime
    , timeOfDay
    , timeZone
    , UTCOffsetHMS(..)
    , timeZoneHMS
    , localToUTCTimeOfDayHMS
    , utcTime
    , zonedTime
    , interval
    ) where

import Prelude as P
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Database.PostgreSQL.Simple.Compat (toPico)
import Database.PostgreSQL.Simple.Time.Interval (Interval(..))
import Data.Attoparsec.ByteString.Char8 as A
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, fromGregorianValid, addDays)
import Data.Time.Clock (UTCTime(..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.Time.LocalTime as Local

-- | Parse a date of the form @YYYY-MM-DD@.
day :: Parser Day
day = do
  y <- decimal <* char '-'
  m <- twoDigits <* char '-'
  d <- twoDigits
  maybe (fail "invalid date") return (fromGregorianValid y m d)

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
  h <- twoDigits <* char ':'
  m <- twoDigits
  mc <- peekChar
  s <- case mc of
         Just ':' -> anyChar *> seconds
         _   -> return 0
  if h < 24 && m < 60 && s <= 60
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit
      return $! parsePicos (fromIntegral real) t
    _ -> return $! fromIntegral real
 where
  parsePicos :: Int64 -> B8.ByteString -> Pico
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where n  = max 0 (12 - B8.length t)
          t' = B8.foldl' (\a c -> 10 * a + fromIntegral (ord c .&. 15)) a0
                         (B8.take 12 t)

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe Local.TimeZone)
timeZone = do
  ch <- satisfy $ \c -> c == '+' || c == '-' || c == 'Z'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      mm <- peekChar
      m <- case mm of
             Just ':'           -> anyChar *> twoDigits
             _                  -> return 0
      let off | ch == '-' = negate off0
              | otherwise = off0
          off0 = h * 60 + m
      case undefined of
        _   | off == 0 ->
              return Nothing
            | h > 23 || m > 59 ->
              fail "invalid time zone offset"
            | otherwise ->
              let !tz = Local.minutesToTimeZone off
              in return (Just tz)

data UTCOffsetHMS = UTCOffsetHMS {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZoneHMS :: Parser (Maybe UTCOffsetHMS)
timeZoneHMS = do
  ch <- satisfy $ \c -> c == '+' || c == '-' || c == 'Z'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      m <- maybeTwoDigits
      s <- maybeTwoDigits
      case undefined of
        _   | h == 0 && m == 0 && s == 0 ->
              return Nothing
            | h > 23 || m >= 60 || s >= 60 ->
              fail "invalid time zone offset"
            | otherwise ->
                if ch == '+'
                then let !tz = UTCOffsetHMS h m s
                      in return (Just tz)
                else let !tz = UTCOffsetHMS (-h) (-m) (-s)
                      in return (Just tz)
  where
    maybeTwoDigits = do
        ch <- peekChar
        case ch of
          Just ':' -> anyChar *> twoDigits
          _        -> return 0

localToUTCTimeOfDayHMS :: UTCOffsetHMS -> Local.TimeOfDay -> (Integer, Local.TimeOfDay)
localToUTCTimeOfDayHMS (UTCOffsetHMS dh dm ds) (Local.TimeOfDay h m s) =
    (\ !a !b -> (a,b)) dday (Local.TimeOfDay h'' m'' s'')
  where
    s' = s - fromIntegral ds
    (!s'', m')
        | s' < 0    = (s' + 60, m - dm - 1)
        | s' >= 60  = (s' - 60, m - dm + 1)
        | otherwise = (s'     , m - dm    )
    (!m'', h')
        | m' < 0    = (m' + 60, h - dh - 1)
        | m' >= 60  = (m' - 60, h - dh + 1)
        | otherwise = (m'     , h - dh    )
    (!h'', dday)
        | h' < 0    = (h' + 24, -1)
        | h' >= 24  = (h' - 24,  1)
        | otherwise = (h'     ,  0)


-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM:SS@.
-- The space may be replaced with a @T@.  The number of seconds may be
-- followed by a fractional component.
localTime :: Parser Local.LocalTime
localTime = Local.LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = satisfy (\c -> c == ' ' || c == 'T')

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
utcTime :: Parser UTCTime
utcTime = do
  (Local.LocalTime d t) <- localTime
  mtz <- timeZoneHMS
  case mtz of
    Nothing -> let !tt = Local.timeOfDayToTime t
               in return (UTCTime d tt)
    Just tz -> let !(dd,t') = localToUTCTimeOfDayHMS tz t
                   !d' = addDays dd d
                   !tt = Local.timeOfDayToTime t'
                in return (UTCTime d' tt)

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM:SS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Parser Local.ZonedTime
zonedTime = Local.ZonedTime <$> localTime <*> (fromMaybe utc <$> timeZone)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""


-- | Parse an interval of the form @[A year[s][ ][B mon[s][ ]][C day[s][ ]][[-]XXX:YY:ZZ[.[Z[Z[Z[Z]]]]]]@.
-- (PosgreSQL default interval output format.)
interval :: Parser Interval
interval = do
  parsedYears <- option 0 $ signed decimal <* string " year" <* optionalS <* optionalSpace
  parsedMonths <- option 0 $ signed decimal <* string " mon" <* optionalS <* optionalSpace
  parsedDays <- option 0 $ signed decimal <* string " day" <* optionalS <* optionalSpace
  parsedMicroseconds <- option 0 $ do
    possibleNegativeSign <- peekChar'
    normalizeSign <- case possibleNegativeSign of '-' -> anyChar *> return negate
                                                  _   -> return id
    parsedHours <- decimal <* char ':'
    parsedMinutes <- twoDigits <* char ':'
    microsecondsOfSeconds <- (*microsecondScale) <$> twoDigits
    maybePartialSeconds <- option Nothing $ Just <$> do
      partialSecondStr <- char '.' *> many1 digit
      let partialSeconds = read $ P.take 6 $ partialSecondStr ++ repeat '0'
      return partialSeconds

    let minutesMicros = microsecondScale * 60 * fromIntegral parsedMinutes
    let hoursMicros = microsecondScale * 3600 * parsedHours
    let parsedMicroseconds = case maybePartialSeconds of Nothing ->
                                                          microsecondsOfSeconds +
                                                          minutesMicros +
                                                          hoursMicros
                                                         Just parsedPartialSecond ->
                                                          microsecondsOfSeconds + parsedPartialSecond +
                                                          minutesMicros +
                                                          hoursMicros

    return $ normalizeSign parsedMicroseconds

  let allMonths = 12 * parsedYears + parsedMonths
  return Interval { intervalMonths = allMonths,
                    intervalDays = parsedDays,
                    intervalMicroseconds = fromIntegral parsedMicroseconds}
    where optionalS = option 's' (char 's')
          optionalSpace = option ' ' space
          microsecondScale = 1000000
