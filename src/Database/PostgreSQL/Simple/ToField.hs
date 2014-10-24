{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.ToField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- The 'ToField' typeclass, for rendering a parameter to a SQL query.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.ToField
    (
      Action(..)
    , ToField(..)
    , toJSONField
    , inQuotes
    , rangeToField
    ) where

import qualified Data.Aeson as JSON
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
                   ( Builder, byteString, char8, stringUtf8
                   , intDec, int8Dec, int16Dec, int32Dec, int64Dec, integerDec
                   , wordDec, word8Dec, word16Dec, word32Dec, word64Dec
                   , floatDec, doubleDec
                   )
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intersperse)
import Data.Monoid (mappend)
import Data.Time (Day, TimeOfDay, LocalTime, UTCTime, ZonedTime, NominalDiffTime)
import Data.Typeable (Typeable)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.Compat (toByteString)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.UUID   (UUID)
import qualified Data.UUID as UUID
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.Range
import           Data.Scientific (Scientific)
#if MIN_VERSION_scientific(0,3,0)
import           Data.Text.Lazy.Builder.Scientific (scientificBuilder)
#else
import           Data.Scientific (scientificBuilder)
#endif
import           Foreign.C.Types (CUInt(..))

-- | How to render an element when substituting it into a query.
data Action =
    Plain Builder
    -- ^ Render without escaping or quoting. Use for non-text types
    -- such as numbers, when you are /certain/ that they will not
    -- introduce formatting vulnerabilities via use of characters such
    -- as spaces or \"@'@\".
  | Escape ByteString
    -- ^ Escape and enclose in quotes before substituting. Use for all
    -- text-like types, and anything else that may contain unsafe
    -- characters when rendered.
  | EscapeByteA ByteString
    -- ^ Escape binary data for use as a @bytea@ literal.  Include surrounding
    -- quotes.  This is used by the 'Binary' newtype wrapper.
  | EscapeIdentifier ByteString
    -- ^ Escape before substituting. Use for all sql identifiers like
    -- table, column names, etc. This is used by the 'Identifier' newtype
    -- wrapper.
  | Many [Action]
    -- ^ Concatenate a series of rendering actions.
    deriving (Typeable)

instance Show Action where
    show (Plain b)            = "Plain " ++ show (toByteString b)
    show (Escape b)           = "Escape " ++ show b
    show (EscapeByteA b)      = "EscapeByteA " ++ show b
    show (EscapeIdentifier b) = "EscapeIdentifier " ++ show b
    show (Many b)             = "Many " ++ show b

-- | A type that may be used as a single parameter to a SQL query.
class ToField a where
    toField :: a -> Action
    -- ^ Prepare a value for substitution into a query string.

instance ToField Action where
    toField a = a
    {-# INLINE toField #-}

instance (ToField a) => ToField (Maybe a) where
    toField Nothing  = renderNull
    toField (Just a) = toField a
    {-# INLINE toField #-}

instance (ToField a) => ToField (In [a]) where
    toField (In []) = Plain $ byteString "(null)"
    toField (In xs) = Many $
        Plain (char8 '(') :
        (intersperse (Plain (char8 ',')) . map toField $ xs) ++
        [Plain (char8 ')')]

renderNull :: Action
renderNull = Plain (byteString "null")

instance ToField Null where
    toField _ = renderNull
    {-# INLINE toField #-}

instance ToField Default where
    toField _ = Plain (byteString "default")
    {-# INLINE toField #-}

instance ToField Bool where
    toField True  = Plain (byteString "true")
    toField False = Plain (byteString "false")
    {-# INLINE toField #-}

instance ToField Int8 where
    toField = Plain . int8Dec
    {-# INLINE toField #-}

instance ToField Int16 where
    toField = Plain . int16Dec
    {-# INLINE toField #-}

instance ToField Int32 where
    toField = Plain . int32Dec
    {-# INLINE toField #-}

instance ToField Int where
    toField = Plain . intDec
    {-# INLINE toField #-}

instance ToField Int64 where
    toField = Plain . int64Dec
    {-# INLINE toField #-}

instance ToField Integer where
    toField = Plain . integerDec
    {-# INLINE toField #-}

instance ToField Word8 where
    toField = Plain . word8Dec
    {-# INLINE toField #-}

instance ToField Word16 where
    toField = Plain . word16Dec
    {-# INLINE toField #-}

instance ToField Word32 where
    toField = Plain . word32Dec
    {-# INLINE toField #-}

instance ToField Word where
    toField = Plain . wordDec
    {-# INLINE toField #-}

instance ToField Word64 where
    toField = Plain . word64Dec
    {-# INLINE toField #-}

instance ToField PQ.Oid where
    toField = Plain . \(PQ.Oid (CUInt x)) -> word32Dec x
    {-# INLINE toField #-}

instance ToField Float where
    toField v | isNaN v || isInfinite v = Plain (inQuotes (floatDec v))
              | otherwise               = Plain (floatDec v)
    {-# INLINE toField #-}

instance ToField Double where
    toField v | isNaN v || isInfinite v = Plain (inQuotes (doubleDec v))
              | otherwise               = Plain (doubleDec v)
    {-# INLINE toField #-}

instance ToField Scientific where
    toField x = toField (LT.toLazyText (scientificBuilder x))
    {-# INLINE toField #-}

instance ToField (Binary SB.ByteString) where
    toField (Binary bs) = EscapeByteA bs
    {-# INLINE toField #-}

instance ToField (Binary LB.ByteString) where
    toField (Binary bs) = (EscapeByteA . SB.concat . LB.toChunks) bs
    {-# INLINE toField #-}

instance ToField Identifier where
    toField (Identifier bs) = EscapeIdentifier (ST.encodeUtf8 bs)
    {-# INLINE toField #-}

instance ToField QualifiedIdentifier where
    toField (QualifiedIdentifier (Just s) t) =
        Many [ EscapeIdentifier (ST.encodeUtf8 s)
             , Plain (char8 '.')
             , EscapeIdentifier (ST.encodeUtf8 t)
             ]
    toField (QualifiedIdentifier Nothing  t) =
               EscapeIdentifier (ST.encodeUtf8 t)
    {-# INLINE toField #-}

instance ToField SB.ByteString where
    toField = Escape
    {-# INLINE toField #-}

instance ToField LB.ByteString where
    toField = toField . SB.concat . LB.toChunks
    {-# INLINE toField #-}

instance ToField ST.Text where
    toField = Escape . ST.encodeUtf8
    {-# INLINE toField #-}

instance ToField [Char] where
    toField = Escape . toByteString . stringUtf8
    {-# INLINE toField #-}

instance ToField LT.Text where
    toField = toField . LT.toStrict
    {-# INLINE toField #-}


instance ToField UTCTime where
    toField = Plain . inQuotes . utcTimeToBuilder
    {-# INLINE toField #-}

instance ToField ZonedTime where
    toField = Plain . inQuotes . zonedTimeToBuilder
    {-# INLINE toField #-}

instance ToField LocalTime where
    toField = Plain . inQuotes . localTimeToBuilder
    {-# INLINE toField #-}

instance ToField Day where
    toField = Plain . inQuotes . dayToBuilder
    {-# INLINE toField #-}

instance ToField TimeOfDay where
    toField = Plain . inQuotes . timeOfDayToBuilder
    {-# INLINE toField #-}

instance ToField UTCTimestamp where
    toField = Plain . inQuotes . utcTimestampToBuilder
    {-# INLINE toField #-}

instance ToField ZonedTimestamp where
    toField = Plain . inQuotes . zonedTimestampToBuilder
    {-# INLINE toField #-}

instance ToField LocalTimestamp where
    toField = Plain . inQuotes . localTimestampToBuilder
    {-# INLINE toField #-}

instance ToField Date where
    toField = Plain . inQuotes . dateToBuilder
    {-# INLINE toField #-}

instance ToField NominalDiffTime where
    toField = Plain . inQuotes . nominalDiffTimeToBuilder
    {-# INLINE toField #-}

instance (ToField a) => ToField (PGArray a) where
    toField xs = Many $
        Plain (byteString "ARRAY[") :
        (intersperse (Plain (char8 ',')) . map toField $ fromPGArray xs) ++
        [Plain (char8 ']')]
        -- Because the ARRAY[...] input syntax is being used, it is possible
        -- that the use of type-specific separator characters is unnecessary.

instance (ToField a) => ToField (Vector a) where
    toField = toField . PGArray . V.toList

-- | Generic range ToField function, useful if you want to define your own
--   range types. Remember not to put your boundary value in quotes and
--   escape double quotes.
rangeToField :: (a -> Action) -> PGRange a -> Action
rangeToField _ (PGRange Unbounded Unbounded) = Plain $ fromByteString "'empty'"
rangeToField f (PGRange a b) = Many $ buildLB a ++ buildUB b
  where
    buildLB Unbounded     = [ Plain $ fromByteString "'(," ]
    buildLB (Inclusive v) = [ Plain $ fromByteString "'[\"", f v
                            , Plain $ fromByteString "\"," ]
    buildLB (Exclusive v) = [ Plain $ fromByteString "'(\"", f v
                            , Plain $ fromByteString "\"," ]
    buildUB Unbounded     = [ Plain $ fromByteString "]'"]
    buildUB (Inclusive v) = [ Plain $ fromChar '"', f v
                            , Plain $ fromByteString "\"]'"]
    buildUB (Exclusive v) = [ Plain $ fromChar '"', f v
                            , Plain $ fromByteString "\")'"]
integralRangeToBuilder :: (Integral a, Show a) => PGRange a -> Builder
integralRangeToBuilder = rangeToBuilder integral
{-# INLINE integralRangeToBuilder #-}

instance ToField (PGRange Int8) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int16) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int32) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Int64) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Integer) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word8) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word16) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word32) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Word64) where
    toField = Plain . integralRangeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Float) where
    toField = Plain . rangeToBuilder float
    {-# INLINE toField #-}

instance ToField (PGRange Double) where
    toField = Plain . rangeToBuilder double
    {-# INLINE toField #-}

instance ToField (PGRange Scientific) where
    toField = Plain . rangeToBuilder f
      where
        f = fromLazyByteString . LT.encodeUtf8 . LT.toLazyText . scientificBuilder
    {-# INLINE toField #-}

instance ToField (PGRange UTCTime) where
    toField = Plain . rangeToBuilder utcTimeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange ZonedTime) where
    toField = Plain . rangeToBuilder zonedTimeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange LocalTime) where
    toField = Plain . rangeToBuilder localTimeToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Day) where
    toField = Plain . rangeToBuilder dayToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange TimeOfDay) where
    toField = Plain . rangeToBuilder timeOfDayToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange UTCTimestamp) where
    toField = Plain . rangeToBuilder utcTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange ZonedTimestamp) where
    toField = Plain . rangeToBuilder zonedTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange LocalTimestamp) where
    toField = Plain . rangeToBuilder localTimestampToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange Date) where
    toField = Plain . rangeToBuilder dateToBuilder
    {-# INLINE toField #-}

instance ToField (PGRange NominalDiffTime) where
    toField = Plain . rangeToBuilder nominalDiffTimeToBuilder
    {-# INLINE toField #-}

instance ToField UUID where
    toField = Plain . inQuotes . byteString . UUID.toASCIIBytes

instance ToField JSON.Value where
    toField = toField . JSON.encode

-- | Convert a Haskell value to a JSON 'JSON.Value' using
-- 'JSON.toJSON' and convert that to a field using 'toField'.
--
-- This can be used as the default implementation for the 'toField'
-- method for Haskell types that have a JSON representation in
-- PostgreSQL.
toJSONField :: JSON.ToJSON a => a -> Action
toJSONField = toField . JSON.toJSON

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where quote = char8 '\''

interleaveFoldr :: (a -> [b] -> [b]) -> b -> [b] -> [a] -> [b]
interleaveFoldr f b bs as = foldr (\a bs -> b : f a bs) bs as
{-# INLINE interleaveFoldr #-}

instance ToRow a => ToField (Values a) where
    toField (Values types rows) =
        case rows of
          []    -> case types of
                     []    -> error norows
                     (_:_) -> values $ typedRow (repeat (lit "null"))
                                                types
                                                [lit " LIMIT 0)"]
          (_:_) -> case types of
                     []    -> values $ untypedRows rows [litC ')']
                     (_:_) -> values $ typedRows rows types [litC ')']
      where
        funcname = "Database.PostgreSQL.Simple.toField :: Values a -> Action"
        norows   = funcname ++ "  either values or types must be non-empty"
        emptyrow = funcname ++ "  each row must contain at least one column"
        lit  = Plain . byteString
        litC = Plain . char8
        values x = Many (lit "(VALUES ": x)

        typedField :: (Action, QualifiedIdentifier) -> [Action] -> [Action]
        typedField (val,typ) rest = val : lit "::" : toField typ : rest

        typedRow :: [Action] -> [QualifiedIdentifier] -> [Action] -> [Action]
        typedRow (val:vals) (typ:typs) rest =
            litC '(' :
              typedField (val,typ) ( interleaveFoldr
                                        typedField
                                        (litC ',')
                                        (litC ')' : rest)
                                        (zip vals typs)   )
        typedRow _ _ _ = error emptyrow

        untypedRow :: [Action] -> [Action] -> [Action]
        untypedRow (val:vals) rest =
            litC '(' : val :
            interleaveFoldr
                 (:)
                 (litC ',')
                 (litC ')' : rest)
                 vals
        untypedRow _ _ = error emptyrow

        typedRows :: ToRow a => [a] -> [QualifiedIdentifier] -> [Action] -> [Action]
        typedRows [] _ _ = error funcname
        typedRows (val:vals) types rest =
            typedRow (toRow val) types (multiRows vals rest)

        untypedRows :: ToRow a => [a] -> [Action] -> [Action]
        untypedRows [] _ = error funcname
        untypedRows (val:vals) rest =
            untypedRow (toRow val) (multiRows vals rest)

        multiRows :: ToRow a => [a] -> [Action] -> [Action]
        multiRows vals rest = interleaveFoldr
                                (untypedRow . toRow)
                                (litC ',')
                                rest
                                vals
