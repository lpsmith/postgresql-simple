{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor  #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables      #-}
{-# LANGUAGE RecordWildCards                         #-}

{- |
Module:      Database.PostgreSQL.Simple.FromField
Copyright:   (c) 2011 MailRank, Inc.
             (c) 2011-2012 Leon P Smith
License:     BSD3
Maintainer:  Leon P Smith <leon@melding-monads.com>
Stability:   experimental
Portability: portable

The 'FromField' typeclass, for converting a single value in a row
returned by a SQL query into a more useful Haskell representation.
Note that each instance of 'FromField' is documented by a list of
compatible postgresql types.

A Haskell numeric type is considered to be compatible with all
PostgreSQL numeric types that are less accurate than it. For instance,
the Haskell 'Double' type is compatible with the PostgreSQL's 32-bit
@int@ type because it can represent a @int@ exactly.  On the other hand,
since a 'Double' might lose precision if representing PostgreSQL's 64-bit
@bigint@, the two are /not/ considered compatible.

Because 'FromField' is a typeclass,  one may provide conversions to
additional Haskell types without modifying postgresql-simple.  This is
particularly useful for supporting PostgreSQL types that postgresql-simple
does not support out-of-box.  Here's an example of what such an instance
might look like for a UUID type that implements the @Read@ class:

@
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple.BuiltinTypes
                 ( BuiltinType(UUID), builtin2oid )
import qualified Data.ByteString as B

instance FromField UUID where
   fromField f mdata =
      if typeOid f /= builtin2oid UUID
        then returnError Incompatible f \"\"
        else case B.unpack \`fmap\` mdata of
               Nothing  -> returnError UnexpectedNull f \"\"
               Just dat ->
                  case [ x | (x,t) <- reads dat, (\"\",\"\") <- lex t ] of
                    [x] -> return x
                    _   -> returnError ConversionError f dat
@

Note that because PostgreSQL's @uuid@ type is built into postgres and is
not provided by an extension,  the 'typeOid' of @uuid@ does not change and
thus we can examine it directly.   Here,  we simply pull the type oid out
of the static table provided by postgresql-simple.

On the other hand if the type is provided by an extension,  such as
@PostGIS@ or @hstore@,  then the 'typeOid' is not stable and can vary from
database to database. In this case it is recommended that FromField
instances use 'typename' instead.

-}


module Database.PostgreSQL.Simple.FromField
    (
      FromField(..)
    , FieldParser
    , Conversion()

    , runConversion
    , conversionMap
    , conversionError
    , ResultError(..)
    , returnError

    , Field
    , typename
    , TypeInfo(..)
    , Attribute(..)
    , typeInfo
    , typeInfoByOid
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
import           Control.Exception (Exception)
import           Data.Attoparsec.Char8 hiding (Result)
import           Data.Bits ((.&.), (.|.), shiftL)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Int (Int16, Int32, Int64)
import           Data.List (foldl')
import           Data.Ratio (Ratio)
import           Data.Time ( UTCTime, ZonedTime, LocalTime, Day, TimeOfDay )
import           Data.Typeable (Typeable, typeOf)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word64)
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.BuiltinTypes
import           Database.PostgreSQL.Simple.Compat
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types (Binary(..), Null(..))
import           Database.PostgreSQL.Simple.TypeInfo as TypeInfo
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TypeInfo
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.Arrays as Arrays
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: String
                                , errSQLTableOid :: Maybe PQ.Oid
                                , errSQLField :: String
                                , errHaskellType :: String
                                , errMessage :: String }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: String
                                  , errSQLTableOid :: Maybe PQ.Oid
                                  , errSQLField :: String
                                  , errHaskellType :: String
                                  , errMessage :: String }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: String
                                    , errSQLTableOid :: Maybe PQ.Oid
                                    , errSQLField :: String
                                    , errHaskellType :: String
                                    , errMessage :: String }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError

left :: Exception a => a -> Conversion b
left = conversionError

type FieldParser a = Field -> Maybe ByteString -> Conversion a

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

-- | Returns the data type name.  This is the preferred way of identifying
--   types that do not have a stable type oid, such as types provided by
--   extensions to PostgreSQL.
--
--   More concretely,  it returns the @typname@ column associated with the
--   type oid in the @pg_type@ table.  First, postgresql-simple will check
--   built-in, static table.   If the type oid is not there, postgresql-simple
--   will check a per-connection cache,  and then finally query the database's
--   meta-schema.

typename :: Field -> Conversion ByteString
typename field = typname <$> typeInfo field

typeInfo :: Field -> Conversion TypeInfo
typeInfo Field{..} = Conversion $ \conn -> do
                       Ok <$> (getTypeInfo conn =<< PQ.ftype result column)

typeInfoByOid :: PQ.Oid -> Conversion TypeInfo
typeInfoByOid oid = Conversion $ \conn -> do
                      Ok <$> (getTypeInfo conn oid)

-- | Returns the name of the column.  This is often determined by a table
--   definition,  but it can be set using an @as@ clause.

name :: Field -> Maybe ByteString
name Field{..} = unsafeDupablePerformIO (PQ.fname result column)

-- | Returns the name of the object id of the @table@ associated with the
--   column,  if any.  Returns 'Nothing' when there is no such table;
--   for example a computed column does not have a table associated with it.
--   Analogous to libpq's @PQftable@.

tableOid :: Field -> Maybe PQ.Oid
tableOid Field{..} = toMaybeOid (unsafeDupablePerformIO (PQ.ftable result column))
  where
     toMaybeOid x
       = if   x == PQ.invalidOid
         then Nothing
         else Just x

-- | If the column has a table associated with it,  this returns the number
--   off the associated table column.   Numbering starts from 0.  Analogous
--   to libpq's @PQftablecol@.

tableColumn :: Field -> Int
tableColumn Field{..} = fromCol (unsafeDupablePerformIO (PQ.ftablecol result column))
  where
    fromCol (PQ.Col x) = fromIntegral x

-- | This returns whether the data was returned in a binary or textual format.
--   Analogous to libpq's @PQfformat@.

format :: Field -> PQ.Format
format Field{..} = unsafeDupablePerformIO (PQ.fformat result column)

-- | For dealing with null values.  Compatible with any postgresql type
--   compatible with type @a@.  Note that the type is not checked if
--   the value is null, although it is inadvisable to rely on this
--   behavior.
instance (FromField a) => FromField (Maybe a) where
    fromField _ Nothing = pure Nothing
    fromField f bs      = Just <$> fromField f bs

-- | compatible with any data type,  but the value must be null
instance FromField Null where
    fromField _ Nothing  = pure Null
    fromField f (Just _) = returnError ConversionFailed f "data is not null"

-- | bool
instance FromField Bool where
    fromField f bs
      | typeOid f /= typoid (TypeInfo.bool) = returnError Incompatible f ""
      | bs == Nothing                 = returnError UnexpectedNull f ""
      | bs == Just "t"                = pure True
      | bs == Just "f"                = pure False
      | otherwise                     = returnError ConversionFailed f ""

-- | \"char\"
instance FromField Char where
    fromField f bs =
        if typeOid f /= typoid (TypeInfo.char)
        then returnError Incompatible f ""
        else case bs of
               Nothing -> returnError UnexpectedNull f ""
               Just bs -> if B.length bs /= 1
                          then returnError ConversionFailed f "length not 1"
                          else return $! (B.head bs)

-- | int2
instance FromField Int16 where
    fromField = atto ok16 $ signed decimal

-- | int2, int4
instance FromField Int32 where
    fromField = atto ok32 $ signed decimal

#if WORD_SIZE_IN_BITS < 64
-- | int2, int4,  and if compiled as 64-bit code,  int8 as well.
-- This library was compiled as 32-bit code.
#else
-- | int2, int4,  and if compiled as 64-bit code,  int8 as well.
-- This library was compiled as 64-bit code.
#endif
instance FromField Int where
    fromField = atto okInt $ signed decimal

-- | int2, int4, int8
instance FromField Int64 where
    fromField = atto ok64 $ signed decimal

-- | int2, int4, int8
instance FromField Integer where
    fromField = atto ok64 $ signed decimal

-- | int2, float4
instance FromField Float where
    fromField = atto ok (realToFrac <$> double)
        where ok = mkCompats [Float4,Int2]

-- | int2, int4, float4, float8
instance FromField Double where
    fromField = atto ok double
        where ok = mkCompats [Float4,Float8,Int2,Int4]

-- | int2, int4, float4, float8, numeric
instance FromField (Ratio Integer) where
    fromField = atto ok rational
        where ok = mkCompats [Float4,Float8,Int2,Int4,Numeric]

unBinary :: Binary t -> t
unBinary (Binary x) = x

-- | bytea, name, text, \"char\", bpchar, varchar, unknown
instance FromField SB.ByteString where
    fromField f dat = if typeOid f == typoid TypeInfo.bytea
                      then unBinary <$> fromField f dat
                      else doFromField f okText' (pure . B.copy) dat

-- | oid
instance FromField PQ.Oid where
    fromField f dat = PQ.Oid <$> atto (mkCompat Oid) decimal f dat

-- | bytea, name, text, \"char\", bpchar, varchar, unknown
instance FromField LB.ByteString where
    fromField f dat = LB.fromChunks . (:[]) <$> fromField f dat

unescapeBytea :: Field -> SB.ByteString
              -> Conversion (Binary SB.ByteString)
unescapeBytea f str = case unsafeDupablePerformIO (PQ.unescapeBytea str) of
       Nothing  -> returnError ConversionFailed f "unescapeBytea failed"
       Just str -> pure (Binary str)

-- | bytea
instance FromField (Binary SB.ByteString) where
    fromField f dat = case format f of
      PQ.Text   -> doFromField f okBinary (unescapeBytea f) dat
      PQ.Binary -> doFromField f okBinary (pure . Binary . B.copy) dat

-- | bytea
instance FromField (Binary LB.ByteString) where
    fromField f dat = Binary . LB.fromChunks . (:[]) . unBinary <$> fromField f dat

-- | name, text, \"char\", bpchar, varchar
instance FromField ST.Text where
    fromField f = doFromField f okText $ (either left pure . ST.decodeUtf8')
    -- FIXME:  check character encoding

-- | name, text, \"char\", bpchar, varchar
instance FromField LT.Text where
    fromField f dat = LT.fromStrict <$> fromField f dat

-- | name, text, \"char\", bpchar, varchar
instance FromField [Char] where
    fromField f dat = ST.unpack <$> fromField f dat

-- | timestamptz
instance FromField UTCTime where
  fromField = ff TypeInfo.timestamptz "UTCTime" parseUTCTime

-- | timestamptz
instance FromField ZonedTime where
  fromField = ff TypeInfo.timestamptz "ZonedTime" parseZonedTime

-- | timestamp
instance FromField LocalTime where
  fromField = ff TypeInfo.timestamp "LocalTime" parseLocalTime

-- | date
instance FromField Day where
  fromField = ff TypeInfo.date "Day" parseDay

-- | time
instance FromField TimeOfDay where
  fromField = ff TypeInfo.time "TimeOfDay" parseTimeOfDay

-- | timestamptz
instance FromField UTCTimestamp where
  fromField = ff TypeInfo.timestamptz "UTCTimestamp" parseUTCTimestamp

-- | timestamptz
instance FromField ZonedTimestamp where
  fromField = ff TypeInfo.timestamptz "ZonedTimestamp" parseZonedTimestamp

-- | timestamp
instance FromField LocalTimestamp where
  fromField = ff TypeInfo.timestamp "LocalTimestamp" parseLocalTimestamp

-- | date
instance FromField Date where
  fromField = ff TypeInfo.date "Date" parseDate

ff :: TypeInfo -> String -> (B8.ByteString -> Either String a)
   -> Field -> Maybe B8.ByteString -> Conversion a
ff pgType hsType parse f mstr =
  if typeOid f /= typoid pgType
  then err Incompatible ""
  else case mstr of
         Nothing -> err UnexpectedNull ""
         Just str -> case parse str of
                       Left msg -> err ConversionFailed msg
                       Right val -> return val
 where
   err errC msg = do
     typnam <- typename f
     left $ errC (B8.unpack typnam)
                 (tableOid f)
                 (maybe "" B8.unpack (name f))
                 hsType
                 msg
{-# INLINE ff #-}

-- | Compatible with both types.  Conversions to type @b@ are
--   preferred,  the conversion to type @a@ will be tried after
--   the 'Right' conversion fails.
instance (FromField a, FromField b) => FromField (Either a b) where
    fromField f dat =   (Right <$> fromField f dat)
                    <|> (Left  <$> fromField f dat)

-- | any postgresql array whose elements are compatible with type @a@
instance (FromField a, Typeable a) => FromField (Vector a) where
    fromField f mdat = do
        info <- typeInfo f
        case info of
          TypeInfo.Array{} ->
              case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> do
                   case parseOnly (fromArray info f) dat of
                     Left  err  -> returnError ConversionFailed f err
                     Right conv -> V.fromList <$> conv
          _ -> returnError Incompatible f ""

fromArray :: (FromField a)
          => TypeInfo -> Field -> Parser (Conversion [a])
fromArray typeInfo f = sequence . (parseIt <$>) <$> array delim
  where
    delim = typdelim (typelem typeInfo)
    fElem = f{ typeOid = typoid (typelem typeInfo) }
    parseIt item = (fromField f' . Just . fmt delim) item
      where f' | Arrays.Array _ <- item = f
               | otherwise              = fElem

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
          => Field -> Compat -> (ByteString -> Conversion a)
          -> Maybe ByteString -> Conversion a
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
            => (String -> Maybe PQ.Oid -> String -> String -> String -> err)
            -> Field -> String -> Conversion a
returnError mkErr f msg = do
  typnam <- typename f
  left $ mkErr (B.unpack typnam)
               (tableOid f)
               (maybe "" B.unpack (name f))
               (show (typeOf (undefined :: a)))
               msg

atto :: forall a. (Typeable a)
     => Compat -> Parser a -> Field -> Maybe ByteString
     -> Conversion a
atto types p0 f dat = doFromField f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Conversion a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> pure v
