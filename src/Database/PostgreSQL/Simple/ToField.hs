{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.ToField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'ToField' typeclass, for rendering a parameter to a SQL query.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.ToField
    (
      Action(..)
    , ToField(..)
    , inQuotes
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Blaze.Text (integral, double, float)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intersperse)
import Data.Monoid (mappend)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Typeable (Typeable)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Database.PostgreSQL.Simple.Types (Binary(..), In(..), Null)
import System.Locale (defaultTimeLocale)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import qualified Database.PostgreSQL.LibPQ as PQ

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
  | EscapeBytea ByteString
    -- ^ Escape binary data for use as a @bytea@ literal.  Include surrounding
    -- quotes.  This is used by the 'Binary' newtype wrapper.
  | Many [Action]
    -- ^ Concatenate a series of rendering actions.
    deriving (Typeable)

instance Show Action where
    show (Plain b)       = "Plain " ++ show (toByteString b)
    show (Escape b)      = "Escape " ++ show b
    show (EscapeBytea b) = "EscapeBytea " ++ show b
    show (Many b)        = "Many " ++ show b

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
    toField (In []) = Plain $ fromByteString "(null)"
    toField (In xs) = Many $
        Plain (fromChar '(') :
        (intersperse (Plain (fromChar ',')) . map toField $ xs) ++
        [Plain (fromChar ')')]

renderNull :: Action
renderNull = Plain (fromByteString "null")

instance ToField Null where
    toField _ = renderNull
    {-# INLINE toField #-}

instance ToField Bool where
    toField True  = Plain (fromByteString "true")
    toField False = Plain (fromByteString "false")
    {-# INLINE toField #-}

instance ToField Int8 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Int16 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Int32 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Int where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Int64 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Integer where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Word8 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Word16 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Word32 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Word where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField Word64 where
    toField = Plain . integral
    {-# INLINE toField #-}

instance ToField PQ.Oid where
    toField = Plain . integral . \(PQ.Oid x) -> x
    {-# INLINE toField #-}

instance ToField Float where
    toField v | isNaN v || isInfinite v = Plain (inQuotes (float v))
              | otherwise               = Plain (float v)
    {-# INLINE toField #-}

instance ToField Double where
    toField v | isNaN v || isInfinite v = Plain (inQuotes (double v))
              | otherwise               = Plain (double v)
    {-# INLINE toField #-}

instance ToField (Binary SB.ByteString) where
    toField (Binary bs) = EscapeBytea bs
    {-# INLINE toField #-}

instance ToField (Binary LB.ByteString) where
    toField (Binary bs) = (EscapeBytea . SB.concat . LB.toChunks) bs
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
    toField = Escape . toByteString . Utf8.fromString
    {-# INLINE toField #-}

instance ToField LT.Text where
    toField = toField . LT.toStrict
    {-# INLINE toField #-}

instance ToField UTCTime where
    toField = Plain . Utf8.fromString . formatTime defaultTimeLocale "'%F %T%Q+00'"
    {-# INLINE toField #-}

instance ToField Day where
    toField = Plain . inQuotes . Utf8.fromString . showGregorian
    {-# INLINE toField #-}

instance ToField TimeOfDay where
    toField = Plain . inQuotes . Utf8.fromString . show
    {-# INLINE toField #-}

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where quote = Utf8.fromChar '\''
