{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances,
    OverloadedStrings #-}

-- |
-- Module:      Database.PostgreSQL.Simple.Param
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- The 'Param' typeclass, for rendering a parameter to a SQL query.

module Database.PostgreSQL.Simple.Param
    (
      Action(..)
    , Param(..)
    , inQuotes
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString,
                                 toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Blaze.Text (integral, double, float)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base16.Lazy as L16
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
  | Many [Action]
    -- ^ Concatenate a series of rendering actions.
    deriving (Typeable)

instance Show Action where
    show (Plain b)  = "Plain " ++ show (toByteString b)
    show (Escape b) = "Escape " ++ show b
    show (Many b)   = "Many " ++ show b

-- | A type that may be used as a single parameter to a SQL query.
class Param a where
    render :: a -> Action
    -- ^ Prepare a value for substitution into a query string.

instance Param Action where
    render a = a
    {-# INLINE render #-}

instance (Param a) => Param (Maybe a) where
    render Nothing  = renderNull
    render (Just a) = render a
    {-# INLINE render #-}

instance (Param a) => Param (In [a]) where
    render (In []) = Plain $ fromByteString "(null)"
    render (In xs) = Many $
        Plain (fromChar '(') :
        (intersperse (Plain (fromChar ',')) . map render $ xs) ++
        [Plain (fromChar ')')]

instance Param (Binary SB.ByteString) where
    render (Binary bs) = Plain $ fromByteString "E'\\\\x" `mappend`
                                 fromByteString (B16.encode bs) `mappend`
                                 fromChar '\''

instance Param (Binary LB.ByteString) where
    render (Binary bs) = Plain $ fromByteString "E'\\\\x" `mappend`
                                 fromLazyByteString (L16.encode bs) `mappend`
                                 fromChar '\''

renderNull :: Action
renderNull = Plain (fromByteString "null")

instance Param Null where
    render _ = renderNull
    {-# INLINE render #-}

instance Param Bool where
    render True  = Plain (fromByteString "true")
    render False = Plain (fromByteString "false")
    {-# INLINE render #-}

instance Param Int8 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int16 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int32 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Int64 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Integer where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word8 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word16 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word32 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word where
    render = Plain . integral
    {-# INLINE render #-}

instance Param Word64 where
    render = Plain . integral
    {-# INLINE render #-}

instance Param PQ.Oid where
    render = Plain . integral . \(PQ.Oid x) -> x
    {-# INLINE render #-}

instance Param Float where
    render v | isNaN v || isInfinite v = renderNull
             | otherwise               = Plain (float v)
    {-# INLINE render #-}

instance Param Double where
    render v | isNaN v || isInfinite v = renderNull
             | otherwise               = Plain (double v)
    {-# INLINE render #-}

instance Param SB.ByteString where
    render = Escape
    {-# INLINE render #-}

instance Param LB.ByteString where
    render = render . SB.concat . LB.toChunks
    {-# INLINE render #-}

instance Param ST.Text where
    render = Escape . ST.encodeUtf8
    {-# INLINE render #-}

instance Param [Char] where
    render = Escape . toByteString . Utf8.fromString
    {-# INLINE render #-}

instance Param LT.Text where
    render = render . LT.toStrict
    {-# INLINE render #-}

instance Param UTCTime where
    render = Plain . Utf8.fromString . formatTime defaultTimeLocale "'%F %T%Q+00'"
    {-# INLINE render #-}

instance Param Day where
    render = Plain . inQuotes . Utf8.fromString . showGregorian
    {-# INLINE render #-}

instance Param TimeOfDay where
    render = Plain . inQuotes . Utf8.fromString . show
    {-# INLINE render #-}

-- | Surround a string with single-quote characters: \"@'@\"
--
-- This function /does not/ perform any other escaping.
inQuotes :: Builder -> Builder
inQuotes b = quote `mappend` b `mappend` quote
  where quote = Utf8.fromChar '\''
