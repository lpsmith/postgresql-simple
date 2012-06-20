------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time.Implementation
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Database.PostgreSQL.Simple.Time.Implementation where

import Prelude hiding (take, (++))
import Blaze.ByteString.Builder(Builder, fromByteString)
import Blaze.ByteString.Builder.Char8(fromChar)
import Blaze.Text.Int(integral)
import Control.Arrow((***))
import Control.Applicative
import Control.Monad(when)
import Data.Bits((.&.))
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Time hiding (getTimeZone, getZonedTime)
import Data.Typeable
import Data.Word(Word8)
import qualified Data.Attoparsec.Char8 as A
import Data.Monoid(Monoid(..))
import Data.Fixed (Pico)
import Unsafe.Coerce

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

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
getDay = do
    yearStr <- A.takeWhile A.isDigit
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

decimal :: Fractional a => B.ByteString -> a
decimal str = toNum str / 10^(B.length str)
{-# INLINE decimal #-}

getTimeOfDay :: A.Parser TimeOfDay
getTimeOfDay = do
    hour   <- digits "hours"
    _      <- A.char ':'
    minute <- digits "minutes"
    _      <- A.char ':'
    second <- digits "seconds"
    subsec <- (A.char '.' *> (decimal <$> A.takeWhile1 A.isDigit)) <|> return 0

    let !picos' = second + subsec

    case makeTimeOfDayValid hour minute picos' of
      Nothing -> fail "invalid time of day"
      Just x  -> return $! x

getLocalTime :: A.Parser LocalTime
getLocalTime = LocalTime <$> getDay <*> (A.char ' ' *> getTimeOfDay)

getLocalTimestamp :: A.Parser LocalTimestamp
getLocalTimestamp = getUnbounded getLocalTime

getTimeZone :: A.Parser TimeZone
getTimeZone = do
    sign  <- A.satisfy (\c -> c == '+' || c == '-')
    hours <- digits "timezone"
    mins  <- (A.char ':' *> digits "timezone minutes") <|> pure 0
    let !absset = 60 * hours + mins
        !offset = if sign == '+' then absset else -absset
    return $! minutesToTimeZone offset

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
    let (!dayDelta,!time') = localToUTCTimeOfDay zone time
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
  if A.isDigit x && A.isDigit y
  then return $! (10 * digit (c2w x) + digit (c2w y))
  else fail (msg ++ " is not 2 digits")
{-# INLINE digits #-}

dayToBuilder :: Day -> Builder
dayToBuilder (toGregorian -> (y,m,d)) = do
    pad4 y ++ fromChar '-' ++ pad2 m ++ fromChar '-' ++ pad2 d

timeOfDayToBuilder :: TimeOfDay -> Builder
timeOfDayToBuilder (TimeOfDay h m s) = do
    pad2 h ++ fromChar ':' ++ pad2 m ++ fromChar ':' ++ showSeconds s

timeZoneToBuilder :: TimeZone -> Builder
timeZoneToBuilder tz
    | m == 0     =  sign h ++ pad2 (abs h)
    | otherwise  =  sign h ++ pad2 (abs h) ++ fromChar ':' ++ pad2 (abs m)
  where
    (h,m) = timeZoneMinutes tz `quotRem` 60
    sign h | h >= 0    = fromChar '+'
           | otherwise = fromChar '-'

utcTimeToBuilder :: UTCTime -> Builder
utcTimeToBuilder (UTCTime day time) =
    dayToBuilder day ++ fromChar ' '
    ++ timeOfDayToBuilder (timeToTimeOfDay time) ++ fromByteString "+00"

zonedTimeToBuilder :: ZonedTime -> Builder
zonedTimeToBuilder (ZonedTime localTime tz) =
    localTimeToBuilder localTime ++ timeZoneToBuilder tz

localTimeToBuilder :: LocalTime -> Builder
localTimeToBuilder (LocalTime day tod) =
    dayToBuilder day ++ fromChar ' ' ++ timeOfDayToBuilder tod

unboundedToBuilder :: (a -> Builder) -> (Unbounded a -> Builder)
unboundedToBuilder finiteToBuilder unbounded
    = case unbounded of
        NegInfinity -> fromByteString "-infinity"
        Finite a    -> finiteToBuilder a
        PosInfinity -> fromByteString  "infinity"

utcTimestampToBuilder :: UTCTimestamp -> Builder
utcTimestampToBuilder = unboundedToBuilder utcTimeToBuilder

zonedTimestampToBuilder :: ZonedTimestamp -> Builder
zonedTimestampToBuilder = unboundedToBuilder zonedTimeToBuilder

localTimestampToBuilder :: LocalTimestamp -> Builder
localTimestampToBuilder = unboundedToBuilder localTimeToBuilder

dateToBuilder  :: Date -> Builder
dateToBuilder  = unboundedToBuilder dayToBuilder

showSeconds :: Pico -> Builder
showSeconds xyz
    | yz == 0   = pad2 x
    | z  == 0   = pad2 x ++ fromChar '.' ++  showD6 y
    | otherwise = pad2 x ++ fromChar '.' ++  pad6   y ++ showD6 z
  where
    -- A kludge to work around the fact that Data.Fixed isn't very fast and
    -- doesn't give me access to the MkFixed constructor.
    (x_,yz) = (unsafeCoerce xyz :: Integer)     `quotRem` 1000000000000
    x = fromIntegral x_ :: Int
    (fromIntegral -> y, fromIntegral -> z) = yz `quotRem` 1000000

pad6 :: Int -> Builder
pad6 xy = let (x,y) = xy `quotRem` 1000
           in pad3 x ++ pad3 y

showD6 :: Int -> Builder
showD6 xy = case xy `quotRem` 1000 of
              (x,0) -> showD3 x
              (x,y) -> pad3 x ++ showD3 y

pad3 :: Int -> Builder
pad3 abc = let (ab,c) = abc `quotRem` 10
               (a,b)  = ab  `quotRem` 10
            in p a ++ p b ++ p c

showD3 :: Int -> Builder
showD3 abc = case abc `quotRem` 100 of
              (a, 0) -> p a
              (a,bc) -> case bc `quotRem` 10 of
                          (b,0) -> p a ++ p b
                          (b,c) -> p a ++ p b ++ p c

-- | p assumes its input is in the range [0..9]
p :: Integral n => n -> Builder
p n = fromChar (w2c (fromIntegral (n + 48)))
{-# INLINE p #-}

-- | pad2 assumes its input is in the range [0..99]
pad2 :: Integral n => n -> Builder
pad2 n = let (a,b) = n `quotRem` 10 in p a ++ p b
{-# INLINE pad2 #-}

-- | pad4 assumes its input is positive
pad4 :: (Integral n, Show n) => n -> Builder
pad4 abcd | abcd >= 10000 = integral abcd
          | otherwise     = p a ++ p b ++ p c ++ p d
  where (ab,cd) = abcd `quotRem` 100
        (a,b)   = ab   `quotRem` 10
        (c,d)   = cd   `quotRem` 10
{-# INLINE pad4 #-}
