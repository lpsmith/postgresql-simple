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
import Data.ByteString.Builder(Builder, byteString, char8, integerDec)
import Control.Arrow((***))
import Control.Applicative
import Data.Bits((.&.))
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Time hiding (getTimeZone, getZonedTime)
import Data.Typeable
import Data.Word(Word8)
import Data.Maybe (fromMaybe)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Database.PostgreSQL.Simple.Compat ((<>))
import Data.Monoid(Monoid(..))
import Data.Fixed (Pico)
import Unsafe.Coerce
import qualified Database.PostgreSQL.Simple.Time.Parser as TP
import Database.PostgreSQL.Simple.Compat (fromPico)

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

toNum :: Num n => B.ByteString -> n
toNum = toNum_ 0
{-# INLINE toNum #-}

toNum_ :: Num n => n -> B.ByteString -> n
toNum_ = B.foldl' (\a c -> 10*a + digit c)

digit :: Num n => Word8 -> n
digit c = fromIntegral (c .&. 0x0f)
{-# INLINE digit #-}

digits :: Num n => String -> A.Parser n
digits msg = do
  x <- A.anyChar
  y <- A.anyChar
  if A.isDigit x && A.isDigit y
  then return $! (10 * digit (c2w x) + digit (c2w y))
  else fail (msg <> " is not 2 digits")
{-# INLINE digits #-}

dayToBuilder :: Day -> Builder
dayToBuilder (toGregorian -> (y,m,d)) = do
    pad4 y <> char8 '-' <> pad2 m <> char8 '-' <> pad2 d

timeOfDayToBuilder :: TimeOfDay -> Builder
timeOfDayToBuilder (TimeOfDay h m s) = do
    pad2 h <> char8 ':' <> pad2 m <> char8 ':' <> showSeconds s

timeZoneToBuilder :: TimeZone -> Builder
timeZoneToBuilder tz
    | m == 0     =  sign h <> pad2 (abs h)
    | otherwise  =  sign h <> pad2 (abs h) <> char8 ':' <> pad2 (abs m)
  where
    (h,m) = timeZoneMinutes tz `quotRem` 60
    sign h | h >= 0    = char8 '+'
           | otherwise = char8 '-'

utcTimeToBuilder :: UTCTime -> Builder
utcTimeToBuilder (UTCTime day time) =
    dayToBuilder day <> char8 ' '
    <> timeOfDayToBuilder (timeToTimeOfDay time) <> byteString "+00"

zonedTimeToBuilder :: ZonedTime -> Builder
zonedTimeToBuilder (ZonedTime localTime tz) =
    localTimeToBuilder localTime <> timeZoneToBuilder tz

localTimeToBuilder :: LocalTime -> Builder
localTimeToBuilder (LocalTime day tod) =
    dayToBuilder day <> char8 ' ' <> timeOfDayToBuilder tod

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
nominalDiffTimeToBuilder xyz
    | yz < 500000 = sign <> integerDec x
    | otherwise   = sign <> integerDec x <> char8 '.' <>  showD6 y
  where
    sign = if xyz >= 0 then mempty else char8 '-'
    -- A kludge to work around the fact that Data.Fixed isn't very fast and
    -- NominalDiffTime doesn't give the MkNominalDiffTime constructor.
    (x,yz) = ((unsafeCoerce (abs xyz) :: Integer) + 500000)  `quotRem` 1000000000000
    (fromIntegral -> y, _z) = yz `quotRem` 1000000

showSeconds :: Pico -> Builder
showSeconds xyz
    | yz == 0   = pad2 x
    | z  == 0   = pad2 x <> char8 '.' <>  showD6 y
    | otherwise = pad2 x <> char8 '.' <>  pad6   y <> showD6 z
  where
    (x_,yz) = fromPico xyz `quotRem` 1000000000000
    x = fromIntegral x_ :: Int
    (fromIntegral -> y, fromIntegral -> z) = yz `quotRem` 1000000

pad6 :: Int -> Builder
pad6 xy = let (x,y) = xy `quotRem` 1000
           in pad3 x <> pad3 y

showD6 :: Int -> Builder
showD6 xy = case xy `quotRem` 1000 of
              (x,0) -> showD3 x
              (x,y) -> pad3 x <> showD3 y

pad3 :: Int -> Builder
pad3 abc = let (ab,c) = abc `quotRem` 10
               (a,b)  = ab  `quotRem` 10
            in p a <> p b <> p c

showD3 :: Int -> Builder
showD3 abc = case abc `quotRem` 100 of
              (a, 0) -> p a
              (a,bc) -> case bc `quotRem` 10 of
                          (b,0) -> p a <> p b
                          (b,c) -> p a <> p b <> p c

-- | p assumes its input is in the range [0..9]
p :: Integral n => n -> Builder
p n = char8 (w2c (fromIntegral (n + 48)))
{-# INLINE p #-}

-- | pad2 assumes its input is in the range [0..99]
pad2 :: Integral n => n -> Builder
pad2 n = let (a,b) = n `quotRem` 10 in p a <> p b
{-# INLINE pad2 #-}

-- | pad4 assumes its input is positive
pad4 :: Integer -> Builder
pad4 abcd | abcd >= 10000 = integerDec abcd
          | otherwise     = p a <> p b <> p c <> p d
  where (ab,cd) = abcd `quotRem` 100
        (a,b)   = ab   `quotRem` 10
        (c,d)   = cd   `quotRem` 10
{-# INLINE pad4 #-}
