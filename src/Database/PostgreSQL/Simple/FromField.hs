{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor  #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards, ScopedTypeVariables      #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell        #-}

{- |
Module:      Database.PostgreSQL.Simple.FromField
Copyright:   (c) 2011 MailRank, Inc.
             (c) 2011-2013 Leon P Smith
License:     BSD3
Maintainer:  Leon P Smith <leon@melding-monads.com>
Stability:   experimental

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

Note that the 'Float' and 'Double' instances use attoparsec's 'double'
conversion routine,  which sacrifices some accuracy for speed.   If you
need accuracy,  consider first converting data to a 'Scientific' or 'Rational'
type,  and then converting to a floating-point type.   If you are defining
your own 'Database.PostgreSQL.Simple.FromRow.FromRow' instances,  this can be
achieved simply by
@'fromRational' '<$>' 'Database.PostgreSQL.Simple.FromRow.field'@,  although
this idiom is additionally compatible with PostgreSQL's @int8@ and @numeric@
types.  If this is unacceptable,  you may find
'Database.PostgreSQL.Simple.FromRow.fieldWith' useful.

Also note that while converting to a 'Double' through the 'Scientific' type
is likely somewhat faster than converting through the 'Rational' type,
the 'Scientific' type has no way to represent @NaN@ and @±Infinity@ values.
Thus,  if you need precision conversion of regular floating point values
and the possibility of receiving these special values from the backend,
stick with 'Rational'.

Because 'FromField' is a typeclass,  one may provide conversions to
additional Haskell types without modifying postgresql-simple.  This is
particularly useful for supporting PostgreSQL types that postgresql-simple
does not support out-of-box.  Here's an example of what such an instance
might look like for a UUID type that implements the @Read@ class:

@
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple.FromField
       ( FromField (fromField) , typeOid, returnError, ResultError (..) )
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, uuid)
import qualified Data.ByteString.Char8 as B

instance FromField UUID where
   fromField f mdata =
      if typeOid f /= typoid uuid
        then returnError Incompatible f \"\"
        else case B.unpack \`fmap\` mdata of
               Nothing  -> returnError UnexpectedNull f \"\"
               Just dat ->
                  case [ x | (x,t) <- reads dat, (\"\",\"\") <- lex t ] of
                    [x] -> return x
                    _   -> returnError ConversionFailed f dat
@

Note that because PostgreSQL's @uuid@ type is built into postgres and is
not provided by an extension,  the 'typeOid' of @uuid@ does not change and
thus we can examine it directly.  One could hard-code the type oid,  or
obtain it by other means, but in this case we simply pull it out of the
static table provided by postgresql-simple.

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
    , pgArrayFieldParser

    , optionalField
    , fromJSONField
    ) where

#include "MachDeps.h"

import GHC.Stack
import           Control.Applicative ( (<|>), (<$>), pure, (*>), (<*) )
import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Exception (Exception)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Parser as JSON (value')
import           Data.Attoparsec.ByteString.Char8 hiding (Result)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Int (Int16, Int32, Int64)
import           Data.IORef (IORef, newIORef)
import           Data.Ratio (Ratio)
import           Data.Time ( UTCTime, ZonedTime, LocalTime, Day, TimeOfDay )
import           Data.Typeable (Typeable, typeOf)
import           Data.Vector (Vector)
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector as V
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.Compat
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.Arrays as Arrays
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.UUID.Types   (UUID)
import qualified Data.UUID.Types as UUID
import           Data.Scientific (Scientific)
import           GHC.Real (infinity, notANumber)

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

left :: (HasCallStack, Exception a) => a -> Conversion b
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
    -- Note that retaining any reference to the 'Field' argument causes
    -- the entire @LibPQ.'PQ.Result'@ to be retained.  Thus, implementations
    -- of 'fromField' should return results that do not refer to this value
    -- after the result have been evaluated to WHNF.
    --
    -- Note that as of @postgresql-simple-0.4.0.0@,  the 'ByteString' value
    -- has already been copied out of the @LibPQ.'PQ.Result'@ before it has
    -- been passed to 'fromField'.  This is because for short strings, it's
    -- cheaper to copy the string than to set up a finalizer.

-- | Returns the data type name.  This is the preferred way of identifying
--   types that do not have a stable type oid, such as types provided by
--   extensions to PostgreSQL.
--
--   More concretely,  it returns the @typname@ column associated with the
--   type oid in the @pg_type@ table.  First, postgresql-simple will check
--   the built-in, static table.   If the type oid is not there,
--   postgresql-simple will check a per-connection cache,  and then
--   finally query the database's meta-schema.

typename :: (HasCallStack) => Field -> Conversion ByteString
typename field = typname <$> typeInfo field

typeInfo :: (HasCallStack) => Field -> Conversion TypeInfo
typeInfo Field{..} = Conversion $ \conn -> do
                       Ok <$> (getTypeInfo conn typeOid)

typeInfoByOid :: (HasCallStack) => PQ.Oid -> Conversion TypeInfo
typeInfoByOid oid = Conversion $ \conn -> do
                      Ok <$> (getTypeInfo conn oid)

-- | Returns the name of the column.  This is often determined by a table
--   definition,  but it can be set using an @as@ clause.

name :: (HasCallStack) => Field -> Maybe ByteString
name Field{..} = unsafeDupablePerformIO (PQ.fname result column)

-- | Returns the name of the object id of the @table@ associated with the
--   column,  if any.  Returns 'Nothing' when there is no such table;
--   for example a computed column does not have a table associated with it.
--   Analogous to libpq's @PQftable@.

tableOid :: (HasCallStack) => Field -> Maybe PQ.Oid
tableOid Field{..} = toMaybeOid (unsafeDupablePerformIO (PQ.ftable result column))
  where
     toMaybeOid x
       = if   x == PQ.invalidOid
         then Nothing
         else Just x

-- | If the column has a table associated with it,  this returns the number
--   off the associated table column.   Numbering starts from 0.  Analogous
--   to libpq's @PQftablecol@.

tableColumn :: (HasCallStack) => Field -> Int
tableColumn Field{..} = fromCol (unsafeDupablePerformIO (PQ.ftablecol result column))
  where
    fromCol (PQ.Col x) = fromIntegral x

-- | This returns whether the data was returned in a binary or textual format.
--   Analogous to libpq's @PQfformat@.

format :: (HasCallStack) => Field -> PQ.Format
format Field{..} = unsafeDupablePerformIO (PQ.fformat result column)

-- | void
instance FromField () where
  fromField f _bs
     | typeOid f /= $(inlineTypoid TI.void) = returnError Incompatible f ""
     | otherwise = pure ()

-- | For dealing with null values.  Compatible with any postgresql type
--   compatible with type @a@.  Note that the type is not checked if
--   the value is null, although it is inadvisable to rely on this
--   behavior.
instance FromField a => FromField (Maybe a) where
    fromField = optionalField fromField

-- | For dealing with SQL @null@ values outside of the 'FromField' class.
--   Alternatively, one could use 'Control.Applicative.optional',  but that
--   also turns type and conversion errors into 'Nothing',  whereas this is
--   more specific and turns only @null@ values into 'Nothing'.

optionalField :: (HasCallStack) => FieldParser a -> FieldParser (Maybe a)
optionalField p f mv =
    case mv of
      Nothing -> pure Nothing
      Just _  -> Just <$> p f mv
{-# INLINE optionalField #-}

-- | compatible with any data type,  but the value must be null
instance FromField Null where
    fromField _ Nothing  = pure Null
    fromField f (Just _) = returnError ConversionFailed f "data is not null"

-- | bool
instance FromField Bool where
    fromField f bs
      | typeOid f /= $(inlineTypoid TI.bool) = returnError Incompatible f ""
      | bs == Nothing                 = returnError UnexpectedNull f ""
      | bs == Just "t"                = pure True
      | bs == Just "f"                = pure False
      | otherwise                     = returnError ConversionFailed f ""

-- | \"char\", bpchar
instance FromField Char where
    fromField f bs =
        if $(mkCompats [TI.char,TI.bpchar]) (typeOid f)
        then case bs of
               Nothing -> returnError UnexpectedNull f ""
               Just bs -> if B.length bs /= 1
                          then returnError ConversionFailed f "length not 1"
                          else return $! (B.head bs)
        else returnError Incompatible f ""

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

-- | int2, float4    (Uses attoparsec's 'double' routine,  for
--   better accuracy convert to 'Scientific' or 'Rational' first)
instance FromField Float where
    fromField = atto ok (realToFrac <$> pg_double)
      where ok = $(mkCompats [TI.float4,TI.int2])

-- | int2, int4, float4, float8  (Uses attoparsec's 'double' routine,  for
--   better accuracy convert to 'Scientific' or 'Rational' first)
instance FromField Double where
    fromField = atto ok pg_double
      where ok = $(mkCompats [TI.float4,TI.float8,TI.int2,TI.int4])

-- | int2, int4, int8, float4, float8, numeric
instance FromField (Ratio Integer) where
    fromField = atto ok pg_rational
      where ok = $(mkCompats [TI.float4,TI.float8,TI.int2,TI.int4,TI.int8,TI.numeric])

-- | int2, int4, int8, float4, float8, numeric
instance FromField Scientific where
     fromField = atto ok rational
      where ok = $(mkCompats [TI.float4,TI.float8,TI.int2,TI.int4,TI.int8,TI.numeric])

unBinary :: (HasCallStack) => Binary t -> t
unBinary (Binary x) = x

pg_double :: (HasCallStack) => Parser Double
pg_double
    =   (string "NaN"       *> pure ( 0 / 0))
    <|> (string "Infinity"  *> pure ( 1 / 0))
    <|> (string "-Infinity" *> pure (-1 / 0))
    <|> double

pg_rational :: (HasCallStack) => Parser Rational
pg_rational
    =   (string "NaN"       *> pure notANumber )
    <|> (string "Infinity"  *> pure infinity   )
    <|> (string "-Infinity" *> pure (-infinity))
    <|> rational

-- | bytea, name, text, \"char\", bpchar, varchar, unknown
instance FromField SB.ByteString where
    fromField f dat = if typeOid f == $(inlineTypoid TI.bytea)
                      then unBinary <$> fromField f dat
                      else doFromField f okText' pure dat

-- | oid
instance FromField PQ.Oid where
    fromField f dat = PQ.Oid <$> atto (== $(inlineTypoid TI.oid)) decimal f dat

-- | bytea, name, text, \"char\", bpchar, varchar, unknown
instance FromField LB.ByteString where
    fromField f dat = LB.fromChunks . (:[]) <$> fromField f dat

unescapeBytea :: (HasCallStack) => Field -> SB.ByteString
              -> Conversion (Binary SB.ByteString)
unescapeBytea f str = case unsafeDupablePerformIO (PQ.unescapeBytea str) of
       Nothing  -> returnError ConversionFailed f "unescapeBytea failed"
       Just str -> pure (Binary str)

-- | bytea
instance FromField (Binary SB.ByteString) where
    fromField f dat = case format f of
      PQ.Text   -> doFromField f okBinary (unescapeBytea f) dat
      PQ.Binary -> doFromField f okBinary (pure . Binary) dat

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

-- | citext
instance FromField (CI ST.Text) where
    fromField f mdat = do
       typ <- typename f
       if typ /= "citext"
         then returnError Incompatible f ""
         else case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> either left (pure . CI.mk)
                                        (ST.decodeUtf8' dat)

-- | citext
instance FromField (CI LT.Text) where
    fromField f mdat = do
       typ <- typename f
       if typ /= "citext"
         then returnError Incompatible f ""
         else case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> either left (pure . CI.mk . LT.fromStrict)
                                        (ST.decodeUtf8' dat)

-- | name, text, \"char\", bpchar, varchar
instance FromField [Char] where
    fromField f dat = ST.unpack <$> fromField f dat

-- | timestamptz
instance FromField UTCTime where
  fromField = ff $(inlineTypoid TI.timestamptz) "UTCTime" parseUTCTime

-- | timestamptz
instance FromField ZonedTime where
  fromField = ff $(inlineTypoid TI.timestamptz) "ZonedTime" parseZonedTime

-- | timestamp
instance FromField LocalTime where
  fromField = ff $(inlineTypoid TI.timestamp) "LocalTime" parseLocalTime

-- | date
instance FromField Day where
  fromField = ff $(inlineTypoid TI.date) "Day" parseDay

-- | time
instance FromField TimeOfDay where
  fromField = ff $(inlineTypoid TI.time) "TimeOfDay" parseTimeOfDay

-- | timestamptz
instance FromField UTCTimestamp where
  fromField = ff $(inlineTypoid TI.timestamptz) "UTCTimestamp" parseUTCTimestamp

-- | timestamptz
instance FromField ZonedTimestamp where
  fromField = ff $(inlineTypoid TI.timestamptz) "ZonedTimestamp" parseZonedTimestamp

-- | timestamp
instance FromField LocalTimestamp where
  fromField = ff $(inlineTypoid TI.timestamp) "LocalTimestamp" parseLocalTimestamp

-- | date
instance FromField Date where
  fromField = ff $(inlineTypoid TI.date) "Date" parseDate

ff :: (HasCallStack) => PQ.Oid -> String -> (B8.ByteString -> Either String a)
   -> Field -> Maybe B8.ByteString -> Conversion a
ff compatOid hsType parse f mstr =
  if typeOid f /= compatOid
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
instance (FromField a, Typeable a) => FromField (PGArray a) where
  fromField = pgArrayFieldParser fromField

pgArrayFieldParser :: (HasCallStack, Typeable a) => FieldParser a -> FieldParser (PGArray a)
pgArrayFieldParser fieldParser f mdat = do
        info <- typeInfo f
        case info of
          TI.Array{} ->
              case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> do
                   case parseOnly (fromArray fieldParser info f) dat of
                     Left  err  -> returnError ConversionFailed f err
                     Right conv -> PGArray <$> conv
          _ -> returnError Incompatible f ""

fromArray :: (HasCallStack) => FieldParser a -> TypeInfo -> Field -> Parser (Conversion [a])
fromArray fieldParser typeInfo f = sequence . (parseIt <$>) <$> array delim
  where
    delim = typdelim (typelem typeInfo)
    fElem = f{ typeOid = typoid (typelem typeInfo) }

    parseIt item =
        fieldParser f' $ if item == Arrays.Plain "NULL" then Nothing else Just item'
      where
        item' = fmt delim item
        f' | Arrays.Array _ <- item = f
           | otherwise              = fElem

instance (FromField a, Typeable a) => FromField (Vector a) where
    fromField f v = V.fromList . fromPGArray <$> fromField f v

instance (FromField a, Typeable a) => FromField (IOVector a) where
    fromField f v = liftConversion . V.unsafeThaw =<< fromField f v

-- | uuid
instance FromField UUID where
    fromField f mbs =
      if typeOid f /= $(inlineTypoid TI.uuid)
      then returnError Incompatible f ""
      else case mbs of
             Nothing -> returnError UnexpectedNull f ""
             Just bs ->
                 case UUID.fromASCIIBytes bs of
                   Nothing -> returnError ConversionFailed f "Invalid UUID"
                   Just uuid -> pure uuid

-- | json
instance FromField JSON.Value where
    fromField f mbs =
      if typeOid f /= $(inlineTypoid TI.json) && typeOid f /= $(inlineTypoid TI.jsonb)
      then returnError Incompatible f ""
      else case mbs of
             Nothing -> returnError UnexpectedNull f ""
             Just bs ->
                 case parseOnly (JSON.value' <* endOfInput) bs of
                   Left  err -> returnError ConversionFailed f err
                   Right val -> pure val

-- | Parse a field to a JSON 'JSON.Value' and convert that into a
-- Haskell value using 'JSON.fromJSON'.
--
-- This can be used as the default implementation for the 'fromField'
-- method for Haskell types that have a JSON representation in
-- PostgreSQL.
--
-- The 'Typeable' constraint is required to show more informative
-- error messages when parsing fails.
--
-- Note that @fromJSONField :: FieldParser ('Maybe' Foo)@ will return
-- @'Nothing'@ on the json @null@ value, and return an exception on SQL @null@
-- value.  Alternatively,  one could write @'optionalField' fromJSONField@
-- that will return @Nothing@ on SQL @null@,  and otherwise will call
-- @fromJSONField :: FieldParser Foo@ and then return @'Just'@ the
-- result value,  or return its exception.  If one would
-- like to return @Nothing@ on both the SQL @null@ and json @null@ values,
-- one way to do it would be to write
-- @\\f mv -> 'Control.Monad.join' '<$>' optionalField fromJSONField f mv@
fromJSONField :: (HasCallStack) => (JSON.FromJSON a, Typeable a) => FieldParser a
fromJSONField f mbBs = do
    value <- fromField f mbBs
    case JSON.fromJSON value of
        JSON.Error err -> returnError ConversionFailed f $
                            "JSON decoding error: " ++ err
        JSON.Success x -> pure x

-- | Compatible with the same set of types as @a@.  Note that
--   modifying the 'IORef' does not have any effects outside
--   the local process on the local machine.
instance FromField a => FromField (IORef a) where
    fromField f v = liftConversion . newIORef =<< fromField f v

-- | Compatible with the same set of types as @a@.  Note that
--   modifying the 'MVar' does not have any effects outside
--   the local process on the local machine.
instance FromField a => FromField (MVar a) where
    fromField f v = liftConversion . newMVar =<< fromField f v

type Compat = PQ.Oid -> Bool

okText, okText', okBinary, ok16, ok32, ok64, okInt :: Compat
okText   = $( mkCompats [ TI.name, TI.text, TI.char,
                          TI.bpchar, TI.varchar ] )
okText'  = $( mkCompats [ TI.name, TI.text, TI.char,
                          TI.bpchar, TI.varchar, TI.unknown ] )
okBinary = (== $( inlineTypoid TI.bytea ))
ok16 = (== $( inlineTypoid TI.int2 ))
ok32 = $( mkCompats [TI.int2,TI.int4] )
ok64 = $( mkCompats [TI.int2,TI.int4,TI.int8] )
#if WORD_SIZE_IN_BITS < 64
okInt = ok32
#else
okInt = ok64
#endif

doFromField :: forall a . (Typeable a, HasCallStack)
          => Field -> Compat -> (ByteString -> Conversion a)
          -> Maybe ByteString -> Conversion a
doFromField f isCompat cvt (Just bs)
    | isCompat (typeOid f) = cvt bs
    | otherwise = returnError Incompatible f "types incompatible"
doFromField f _ _ _ = returnError UnexpectedNull f ""


-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError :: forall a err . (Typeable a, Exception err, HasCallStack)
            => (String -> Maybe PQ.Oid -> String -> String -> String -> err)
            -> Field -> String -> Conversion a
returnError mkErr f msg = do
  typnam <- typename f
  left $ mkErr (B.unpack typnam)
               (tableOid f)
               (maybe "" B.unpack (name f))
               (show (typeOf (undefined :: a)))
               msg

atto :: forall a. (Typeable a, HasCallStack)
     => Compat -> Parser a -> Field -> Maybe ByteString
     -> Conversion a
atto types p0 f dat = doFromField f types (go p0) dat
  where
    go :: Parser a -> ByteString -> Conversion a
    go p s =
        case parseOnly p s of
          Left err -> returnError ConversionFailed f err
          Right  v -> pure v
