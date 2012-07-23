{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor  #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables      #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.FromField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'FromField' typeclass, for converting a single value in a row
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

module Database.PostgreSQL.Simple.FromField
    (
      FromField(..)
    , FieldParser
    , ResultError(..)
    , returnError

    , Field
    , typename
    , name
    , tableOid
    , tableColumn
    , format
    , typeOid
    , PQ.Oid(..)
    , PQ.Format(..)
    ) where

#include "MachDeps.h"

import           Control.Applicative
                   ( Applicative, (<|>), (<$>), pure )
import           Control.Exception (SomeException(..), Exception)
import           Data.Attoparsec.Char8 hiding (Result)
import           Data.Bits ((.&.), (.|.), shiftL)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Int (Int16, Int32, Int64)
import           Data.List (foldl')
import           Data.Ratio (Ratio)
import           Data.Time ( UTCTime, ZonedTime, LocalTime, Day, TimeOfDay )
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64)
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.BuiltinTypes
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types (Binary(..), Null(..))
import           Database.PostgreSQL.Simple.Time
import qualified Database.PostgreSQL.LibPQ as PQ
import           System.IO.Unsafe (unsafePerformIO)
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

left :: Exception a => a -> Ok b
left = Errors . (:[]) . SomeException

type FieldParser a = Field -> Maybe ByteString -> Ok a

-- | A type that may be converted from a SQL type.
class FromField a where
    fromField :: FieldParser a
    -- ^ Convert a SQL value to a Haskell value.
    --
    -- Returns a list of exceptions if the conversion fails.  In the case of
    -- library instances,  this will usually be a single 'ResultError',  but
    -- may be a 'UnicodeException'.
    --
    -- Implementations of 'fromField' should not retain any references to
    -- the 'Field' nor the 'ByteString' arguments after the result has
    -- been evaluated to WHNF.  Such a reference causes the entire
    -- @LibPQ.'PQ.Result'@ to be retained.
    --
    -- For example,  the instance for 'ByteString' uses 'B.copy' to avoid
    -- such a reference,  and that using bytestring functions such as 'B.drop'
    -- and 'B.takeWhile' alone will also trigger this memory leak.

instance (FromField a) => FromField (Maybe a) where
    fromField _ Nothing = pure Nothing
    fromField f bs      = Just <$> fromField f bs

instance FromField Null where
    fromField _ Nothing  = pure Null
    fromField f (Just _) = returnError ConversionFailed f "data is not null"

instance FromField Bool where
    fromField f bs
      | typeOid f /= builtin2oid Bool = returnError Incompatible f ""
      | bs == Nothing                 = returnError UnexpectedNull f ""
      | bs == Just "t"                = pure True
      | bs == Just "f"                = pure False
      | otherwise                     = returnError ConversionFailed f ""

instance FromField Int16 where
    fromField = atto ok16 $ signed decimal

instance FromField Int32 where
    fromField = atto ok32 $ signed decimal

instance FromField Int where
    fromField = atto okInt $ signed decimal

instance FromField Int64 where
    fromField = atto ok64 $ signed decimal

instance FromField Integer where
    fromField = atto ok64 $ signed decimal

instance FromField Float where
    fromField = atto ok (realToFrac <$> double)
        where ok = mkCompats [Float4,Int2]

instance FromField Double where
    fromField = atto ok double
        where ok = mkCompats [Float4,Float8,Int2,Int4]

instance FromField (Ratio Integer) where
    fromField = atto ok rational
        where ok = mkCompats [Float4,Float8,Int2,Int4,Numeric]

unBinary :: Binary t -> t
unBinary (Binary x) = x

instance FromField SB.ByteString where
    fromField f dat = if typeOid f == builtin2oid ByteA
                      then unBinary <$> fromField f dat
                      else doFromField f okText' (pure . B.copy) dat

instance FromField PQ.Oid where
    fromField f dat = PQ.Oid <$> atto (mkCompat Oid) decimal f dat

instance FromField LB.ByteString where
    fromField f dat = LB.fromChunks . (:[]) <$> fromField f dat

unescapeBytea :: Field -> SB.ByteString
              -> Ok (Binary SB.ByteString)
unescapeBytea f str = case unsafePerformIO (PQ.unescapeBytea str) of
       Nothing  -> returnError ConversionFailed f "unescapeBytea failed"
       Just str -> pure (Binary str)

instance FromField (Binary SB.ByteString) where
    fromField f dat = case format f of
      PQ.Text   -> doFromField f okBinary (unescapeBytea f) dat
      PQ.Binary -> doFromField f okBinary (pure . Binary . B.copy) dat

instance FromField (Binary LB.ByteString) where
    fromField f dat = Binary . LB.fromChunks . (:[]) . unBinary <$> fromField f dat

instance FromField ST.Text where
    fromField f = doFromField f okText $ (either left pure . ST.decodeUtf8')
    -- FIXME:  check character encoding

instance FromField LT.Text where
    fromField f dat = LT.fromStrict <$> fromField f dat

instance FromField [Char] where
    fromField f dat = ST.unpack <$> fromField f dat

instance FromField UTCTime where
  fromField = ff TimestampTZ "UTCTime" parseUTCTime

instance FromField ZonedTime where
  fromField = ff TimestampTZ "ZonedTime" parseZonedTime

instance FromField LocalTime where
  fromField = ff Timestamp "LocalTime" parseLocalTime

instance FromField Day where
  fromField = ff Date "Day" parseDay

instance FromField TimeOfDay where
  fromField = ff Time "TimeOfDay" parseTimeOfDay

instance FromField UTCTimestamp where
  fromField = ff TimestampTZ "UTCTimestamp" parseUTCTimestamp

instance FromField ZonedTimestamp where
  fromField = ff TimestampTZ "ZonedTimestamp" parseZonedTimestamp

instance FromField LocalTimestamp where
  fromField = ff Timestamp "LocalTimestamp" parseLocalTimestamp

instance FromField Date where
  fromField = ff Date "Date" parseDate

ff :: BuiltinType -> String -> (B8.ByteString -> Either String a)
   -> Field -> Maybe B8.ByteString -> Ok a
ff pgType hsType parse f mstr
    | typeOid f /= builtin2oid pgType
    = left (Incompatible   (B8.unpack (typename f)) hsType "")
    | Nothing <- mstr
    = left (UnexpectedNull (B8.unpack (typename f)) hsType "")
    | Just str <- mstr
    = case parse str of
        Left msg -> left (ConversionFailed (B8.unpack (typename f)) hsType msg)
        Right val -> return val
{-# INLINE ff #-}

instance (FromField a, FromField b) => FromField (Either a b) where
    fromField f dat =   (Right <$> fromField f dat)
                    <|> (Left  <$> fromField f dat)

newtype Compat = Compat Word64

mkCompats :: [BuiltinType] -> Compat
mkCompats = foldl' f (Compat 0) . map mkCompat
  where f (Compat a) (Compat b) = Compat (a .|. b)

mkCompat :: BuiltinType -> Compat
mkCompat = Compat . shiftL 1 . fromEnum

compat :: Compat -> Compat -> Bool
compat (Compat a) (Compat b) = a .&. b /= 0

okText, okText', okBinary, ok16, ok32, ok64, okInt :: Compat
okText   = mkCompats [Name,Text,Char,BpChar,VarChar]
okText'  = mkCompats [Name,Text,Char,BpChar,VarChar,Unknown]
okBinary = mkCompats [ByteA]
ok16 = mkCompats [Int2]
ok32 = mkCompats [Int2,Int4]
ok64 = mkCompats [Int2,Int4,Int8]
#if WORD_SIZE_IN_BITS < 64
okInt = ok32
#else
okInt = ok64
#endif

doFromField :: forall a . (Typeable a)
          => Field -> Compat -> (ByteString -> Ok a)
          -> Maybe ByteString -> Ok a
doFromField f types cvt (Just bs)
    | Just typ <- oid2builtin (typeOid f)
    , mkCompat typ `compat` types = cvt bs
    | otherwise = returnError Incompatible f "types incompatible"
doFromField f _ _ _ = returnError UnexpectedNull f ""


-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError :: forall a err . (Typeable a, Exception err)
            => (String -> String -> String -> err)
            -> Field -> String -> Ok a
returnError mkErr f = left . mkErr (B.unpack (typename f))
                                   (show (typeOf (undefined :: a)))

atto :: forall a. (Typeable a)
     => Compat -> Parser a -> Field -> Maybe ByteString
     -> Ok a
atto types p0 f dat = doFromField f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Ok a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> pure v
