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
-- PostgreSQL numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the PostgreSQL's 32-bit
-- @Int@ type because it can represent a @Int@ exactly. On the other hand,
-- since a 'Double' might lose precision if representing a 64-bit @BigInt@,
-- the two are /not/ considered compatible.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Result
    (
      Result(..)
    , ResultError(..)
    , returnError
    ) where

#include "MachDeps.h"

import Control.Applicative (Applicative, (<$>), (<*>), (<*), pure)
import Control.Exception (SomeException(..), Exception)
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
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word64)
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.BuiltinTypes
import Database.PostgreSQL.Simple.Types (Binary(..), Null(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import System.IO.Unsafe (unsafePerformIO)
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

type Status = Either SomeException

left :: Exception a => a -> Status b
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

instance Result Null where
    convert _ Nothing  = pure Null
    convert f (Just _) = returnError ConversionFailed f "data is not null"

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

instance Result Float where
    convert = atto ok (realToFrac <$> double)
        where ok = mkCompats [Float4,Int2]

instance Result Double where
    convert = atto ok double
        where ok = mkCompats [Float4,Float8,Int2,Int4]

instance Result (Ratio Integer) where
    convert = atto ok rational
        where ok = mkCompats [Float4,Float8,Int2,Int4,Numeric]

unBinary :: Binary t -> t
unBinary (Binary x) = x

instance Result SB.ByteString where
    convert f dat = if typeOid f == builtin2oid Bytea
                      then unBinary <$> convert f dat
                      else doConvert f okText' (pure . B.copy) dat

instance Result PQ.Oid where
    convert f dat = PQ.Oid <$> atto (mkCompat Oid) decimal f dat

instance Result LB.ByteString where
    convert f dat = LB.fromChunks . (:[]) <$> convert f dat

unescapeBytea :: Field -> SB.ByteString
              -> Status (Binary SB.ByteString)
unescapeBytea f str = case unsafePerformIO (PQ.unescapeBytea str) of
       Nothing  -> returnError ConversionFailed f "unescapeBytea failed"
       Just str -> pure (Binary str)

instance Result (Binary SB.ByteString) where
    convert f dat = case format f of
      PQ.Text   -> doConvert f okBinary (unescapeBytea f) dat
      PQ.Binary -> doConvert f okBinary (pure . Binary . B.copy) dat

instance Result (Binary LB.ByteString) where
    convert f dat = Binary . LB.fromChunks . (:[]) . unBinary <$> convert f dat

instance Result ST.Text where
    convert f = doConvert f okText $ (either left Right . ST.decodeUtf8')
    -- FIXME:  check character encoding

instance Result LT.Text where
    convert f dat = LT.fromStrict <$> convert f dat

instance Result [Char] where
    convert f dat = ST.unpack <$> convert f dat

instance Result UTCTime where
    convert f =
        case oid2builtin (typeOid f) of
          Just Timestamp             -> doIt "%F %T%Q"   id
          Just TimestampWithTimeZone -> doIt "%F %T%Q%z" (++ "00")
          _ -> const $ returnError Incompatible f "types incompatible"
        where
          doIt _   _          Nothing   = returnError UnexpectedNull f ""
          doIt fmt preprocess (Just bs) =
              case parseTime defaultTimeLocale fmt str of
                Just t  -> Right t
                Nothing -> returnError ConversionFailed f "could not parse"
              where str = preprocess (B8.unpack bs)

instance Result Day where
    convert f = atto ok date f
        where ok = mkCompats [Date]
              date = fromGregorian <$> (decimal <* char '-')
                                   <*> (decimal <* char '-')
                                   <*> decimal

instance Result TimeOfDay where
    convert f = atto' ok time f
        where ok = mkCompats [Time]
              time = do
                hours <- decimal <* char ':'
                mins <- decimal <* char ':'
                secs <- decimal :: Parser Int
                case makeTimeOfDayValid hours mins (fromIntegral secs) of
                  Just t -> return (pure t)
                  _      -> return (returnError ConversionFailed f "could not parse")

instance (Result a, Result b) => Result (Either a b) where
    convert f dat = case convert f dat of
                      Right x -> Right (Right x)
                      Left  _ -> case convert f dat of
                                   Right x -> Right (Left x)
                                   Left  e -> Left e

newtype Compat = Compat Word64

mkCompats :: [BuiltinType] -> Compat
mkCompats = foldl' f (Compat 0) . map mkCompat
  where f (Compat a) (Compat b) = Compat (a .|. b)

mkCompat :: BuiltinType -> Compat
mkCompat = Compat . shiftL 1 . fromEnum

compat :: Compat -> Compat -> Bool
compat (Compat a) (Compat b) = a .&. b /= 0

okText, okText', okBinary, ok16, ok32, ok64, okInt :: Compat
okText   = mkCompats [Name,Text,Char,Bpchar,Varchar]
okText'  = mkCompats [Name,Text,Char,Bpchar,Varchar,Unknown]
okBinary = mkCompats [Bytea]
ok16 = mkCompats [Int2]
ok32 = mkCompats [Int2,Int4]
ok64 = mkCompats [Int2,Int4,Int8]
#if WORD_SIZE_IN_BITS < 64
okInt = ok32
#else
okInt = ok64
#endif

doConvert :: forall a . (Typeable a)
          => Field -> Compat -> (ByteString -> Status a)
          -> Maybe ByteString -> Status a
doConvert f types cvt (Just bs)
    | Just typ <- oid2builtin (typeOid f)
    , mkCompat typ `compat` types = cvt bs
    | otherwise = returnError Incompatible f "types incompatible"
doConvert f _ _ _ = returnError UnexpectedNull f ""


-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError :: forall a err . (Typeable a, Exception err)
            => (String -> String -> String -> err)
            -> Field -> String -> Either SomeException a
returnError mkErr f = left . mkErr (B.unpack (typename f))
                                   (show (typeOf (undefined :: a)))

atto :: forall a. (Typeable a)
     => Compat -> Parser a -> Field -> Maybe ByteString
     -> Status a
atto types p0 f dat = doConvert f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Status a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> Right v

atto' :: forall a. (Typeable a)
     => Compat -> Parser (Status a) -> Field -> Maybe ByteString
     -> Status a
atto' types p0 f dat = doConvert f types (go p0) dat
  where
    go :: Parser (Status a) -> ByteString -> Status a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> v

instance Result RawResult where
   convert field rawData = Right (RawResult field rawData)
