{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.QueryResults
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'Result' typeclass, for converting a single value in a row
-- returned by a SQL query into a more useful Haskell representation.
--
-- A Haskell numeric type is considered to be compatible with all
-- MySQL numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the MySQL 'Long' type
-- because it can represent a 'Long' exactly. On the other hand, since
-- a 'Double' might lose precision if representing a 'LongLong', the
-- two are /not/ considered compatible.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Result
    (
      Result(..)
    , ResultError(..)
    , Status(..)
    ) where

#include "MachDeps.h"

import Control.Applicative (Applicative, (<$>), (<*>), (<*), pure)
import Control.Exception (Exception, throw)
import Data.Attoparsec.Char8 hiding (Result)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int16, Int32, Int64)
import Data.List (foldl')
import Data.Ratio (Ratio)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime)
import Data.Time.LocalTime (TimeOfDay, makeTimeOfDayValid)
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word ({- Word, Word8, Word16, Word32, -} Word64)
-- import Database.MySQL.Base.Types (Field(..), Type(..))
import Database.PostgreSQL.Simple.Implementation
import Database.PostgreSQL.Simple.Field (Field(..), RawResult(..))
import Database.PostgreSQL.Simple.BuiltinTypes
import qualified Database.PostgreSQL.LibPQ as PQ
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: String
                                , errHaskellType :: String
                                , errMessage :: String }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: String
                                  , errHaskellType :: String
                                  , errMessage :: String }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: String
                                    , errHaskellType :: String
                                    , errMessage :: String }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError

-- | A type that may be converted from a SQL type.
class Result a where
    convert :: Field -> Maybe ByteString -> Status ResultError a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Throws a 'ResultError' if conversion fails.
{--
instance (Result a) => Result (Maybe a) where
    convert _ Nothing = pure Nothing
    convert f bs      = Just <$> convert f bs

instance Result Bool where
    convert = atto ok8 ((/=(0::Int)) <$> decimal)

instance Result Int8 where
    convert = atto ok8 $ signed decimal
--}
instance Result Int16 where
    convert = atto ok16 $ signed decimal

instance Result Int32 where
    convert = atto ok32 $ signed decimal

instance Result Int where
    convert = atto okInt $ signed decimal

instance Result Int64 where
    convert = atto ok64 $ signed decimal

instance Result Integer where
    convert = atto ok64 $ signed decimal
{--
instance Result Word8 where
    convert = atto ok8 decimal

instance Result Word16 where
    convert = atto ok16 decimal

instance Result Word32 where
    convert = atto ok32 decimal

instance Result Word where
    convert = atto okWord decimal

instance Result Word64 where
    convert = atto ok64 decimal
--}
{--
instance Result Float where
    convert = atto ok (realToFrac <$> double)
        where ok = mkCompats [Float,Double,Decimal,NewDecimal,Tiny,Short,Int24]

instance Result Double where
    convert = atto ok double
        where ok = mkCompats [Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,
                              Long]
--}
{--
instance Result (Ratio Integer) where
    convert = atto ok rational
        where ok = mkCompats [Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,
                              Long,LongLong]
--}
instance Result SB.ByteString where
    convert f = doConvert f okText $ pure

instance Result PQ.Oid where
    convert f dat = PQ.Oid <$> atto (mkCompat Oid) decimal f dat

instance Result LB.ByteString where
    convert f dat = LB.fromChunks . (:[]) <$> convert f dat


instance Result ST.Text where
    convert f {- | isText f -} = doConvert f okText $ (Success . ST.decodeUtf8)
{-              | otherwise = incompatible f (typeOf ST.empty)
                            "attempt to mix binary and text" -}
    -- FIXME:  return a "Fail someUnicodeError" instead of throwing it
    --         check character encoding

instance Result LT.Text where
    convert f dat = LT.fromStrict <$> convert f dat

instance Result [Char] where
    convert f dat = ST.unpack <$> convert f dat

instance Result UTCTime where
    convert f = doConvert f ok $ \bs ->
        case parseTime defaultTimeLocale "%F %T%Q%z" (B8.unpack bs ++ "00") of
          Just t -> Success t
          Nothing -> conversionFailed f "UTCTime" "could not parse"
      where ok = mkCompats [TimestampWithTimeZone]
{--
instance Result Day where
    convert f = flip (atto ok) f $ case fieldType f of
                                     Year -> year
                                     _    -> date
        where ok = mkCompats [Year,Date,NewDate]
              year = fromGregorian <$> decimal <*> pure 1 <*> pure 1
              date = fromGregorian <$> (decimal <* char '-')
                                   <*> (decimal <* char '-')
                                   <*> decimal

instance Result TimeOfDay where
    convert f = flip (atto ok) f $ do
                hours <- decimal <* char ':'
                mins <- decimal <* char ':'
                secs <- decimal :: Parser Int
                case makeTimeOfDayValid hours mins (fromIntegral secs) of
                  Just t -> return t
                  _      -> conversionFailed f "TimeOfDay" "could not parse"
        where ok = mkCompats [Time]

isText :: Field -> Bool
isText f = fieldCharSet f /= 63
--}
newtype Compat = Compat Word64

mkCompats :: [BuiltinType] -> Compat
mkCompats = foldl' f (Compat 0) . map mkCompat
  where f (Compat a) (Compat b) = Compat (a .|. b)

mkCompat :: BuiltinType -> Compat
mkCompat = Compat . shiftL 1 . fromEnum

compat :: Compat -> Compat -> Bool
compat (Compat a) (Compat b) = a .&. b /= 0
{--
okText, ok8, ok16, ok32, ok64, okWord :: Compat
--}
okText = mkCompats [Name,Text,Char,Bpchar,Varchar]
{--}
--ok8  = mkCompats [Tiny]
ok16 = mkCompats [Int2]
ok32 = mkCompats [Int2,Int4]
ok64 = mkCompats [Int2,Int4,Int8]
#if WORD_SIZE_IN_BITS < 64
okInt = ok32
#else
okInt = ok64
#endif
--}
doConvert :: forall a . (Typeable a)
          => Field -> Compat -> (ByteString -> Status ResultError a)
          -> Maybe ByteString -> Status ResultError a
doConvert f types cvt (Just bs)
    | Just typ <- oid2builtin (typeOid f)
    , mkCompat typ `compat` types = cvt bs
    | otherwise = incompatible f (typeOf (undefined::a)) "types incompatible"
doConvert f _ cvt _ = Fail $ UnexpectedNull (B.unpack (typename f))
                              (show (typeOf (undefined::a))) ""

incompatible :: Field -> TypeRep -> String -> Status ResultError a
incompatible f r = Fail . Incompatible (B.unpack (typename f)) (show r)

conversionFailed :: Field -> String -> String -> Status ResultError a
conversionFailed f s = Fail . ConversionFailed (B.unpack (typename f)) s

atto :: (Typeable a) => Compat -> Parser a -> Field -> Maybe ByteString -> Status ResultError a
atto types p0 f dat = doConvert f types (go undefined p0) dat
  where
    go :: (Typeable a) => a -> Parser a -> ByteString -> Status ResultError a
    go dummy p s =
        case parseOnly p s of
          Left err -> conversionFailed f (show (typeOf dummy)) err
          Right v  -> Success v

instance Result RawResult where
   convert field rawData = Success (RawResult field rawData)
