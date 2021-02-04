{-# LANGUAGE CPP #-}
-- | This is a module of its own, partly because it uses the CPP extension,
-- which doesn't play well with backslash-broken string literals.
module Database.PostgreSQL.Simple.Compat
    ( mask
    , (<>)
    , unsafeDupablePerformIO
    , toByteString
    , scientificBuilder
    , toPico
    , fromPico
    ) where

import qualified Control.Exception as E
import Data.Monoid
import Data.ByteString         (ByteString)
#if MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Lazy    (toStrict)
#else
import qualified Data.ByteString as B
import Data.ByteString.Lazy    (toChunks)
#endif
import Data.ByteString.Builder (Builder, toLazyByteString)

#if MIN_VERSION_scientific(0,3,0)
import Data.Text.Lazy.Builder.Scientific (scientificBuilder)
#else
import Data.Scientific (scientificBuilder)
#endif

import System.IO.Unsafe (unsafeDupablePerformIO)

import Data.Fixed (Pico)
import Data.Fixed (Fixed(MkFixed))

-- | Like 'E.mask', but with a monomorphic restore callback, unlike in
-- 'E.mask'.  This could be fixed by changing the type signature, but
-- it would require us to enable the RankNTypes extension (since
-- 'E.mask' has a rank-3 type).  The 'withTransactionMode' function
-- calls the restore callback only once, so we don't need that
-- polymorphism.
mask :: ((IO a -> IO a) -> IO b) -> IO b
mask io = E.mask $ \restore -> io restore
{-# INLINE mask #-}

toByteString :: Builder -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
toByteString x = toStrict (toLazyByteString x)
#else
toByteString x = B.concat (toChunks (toLazyByteString x))
#endif

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
