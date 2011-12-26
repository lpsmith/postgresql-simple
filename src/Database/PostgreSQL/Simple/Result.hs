{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables, OverloadedStrings #-}
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
    ) where

#include "MachDeps.h"

import Control.Applicative (Applicative, (<$>), (<*>), (<*), pure)
import Control.Exception (SomeException(..), Exception, throw)
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
import Data.Word (Word64)
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Field (Field(..), RawResult(..))
import Database.PostgreSQL.Simple.BuiltinTypes
import qualified Database.PostgreSQL.LibPQ as PQ
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Encoding.Error (UnicodeException)
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

left :: Exception a => a -> Either SomeException b
left = Left . SomeException

-- | A type that may be converted from a SQL type.
class Result a where
    convert :: Field -> Maybe ByteString -> Either SomeException a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Returns an exception if the conversion fails.  In the case of
    -- library instances,  this will usually be a 'ResultError',  but may
    -- be a 'UnicodeException'.

instance (Result a) => Result (Maybe a) where
    convert _ Nothing = pure Nothing
    convert f bs      = Just <$> convert f bs

instance Result Bool where
    convert f bs
      | typeOid f /= builtin2oid Bool = returnError Incompatible f ""
      | bs == Nothing                 = returnError UnexpectedNull f ""
      | bs == Just "t"                = pure True
      | bs == Just "f"                = pure False
      | otherwise                     = returnError ConversionFailed f ""

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
    convert f = doConvert f okText $ (either left Right . ST.decodeUtf8')
{-              | isText f  = doConvert f okText $ (Right . ST.decodeUtf8)
                | otherwise = incompatible f (typeOf ST.empty)
                            "attempt to mix binary and text" -}
    -- FIXME:  check character encoding

instance Result LT.Text where
    convert f dat = LT.fromStrict <$> convert f dat

instance Result [Char] where
    convert f dat = ST.unpack <$> convert f dat

instance Result UTCTime where
    convert f = doConvert f ok $ \bs ->
        case parseTime defaultTimeLocale "%F %T%Q%z" (B8.unpack bs ++ "00") of
          Just t -> Right t
          Nothing -> returnError ConversionFailed f "could not parse"
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

okText, ok16, ok32, ok64 :: Compat
okText = mkCompats [Name,Text,Char,Bpchar,Varchar]
ok16 = mkCompats [Int2]
ok32 = mkCompats [Int2,Int4]
ok64 = mkCompats [Int2,Int4,Int8]
#if WORD_SIZE_IN_BITS < 64
okInt = ok32
#else
okInt = ok64
#endif

doConvert :: forall a . (Typeable a)
          => Field -> Compat -> (ByteString -> Either SomeException a)
          -> Maybe ByteString -> Either SomeException a
doConvert f types cvt (Just bs)
    | Just typ <- oid2builtin (typeOid f)
    , mkCompat typ `compat` types = cvt bs
    | otherwise = returnError Incompatible f "types incompatible"
doConvert f _ _ _ = returnError UnexpectedNull f ""


returnError :: forall a err . (Typeable a, Exception err)
            => (String -> String -> String -> err)
            -> Field -> String -> Either SomeException a
returnError mkErr f = left . mkErr (B.unpack (typename f))
                                   (show (typeOf (undefined :: a)))

atto :: forall a. (Typeable a)
     => Compat -> Parser a -> Field -> Maybe ByteString
     -> Either SomeException a
atto types p0 f dat = doConvert f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Either SomeException a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> Right v

instance Result RawResult where
   convert field rawData = Right (RawResult field rawData)
