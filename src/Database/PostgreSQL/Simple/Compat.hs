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

#if   __GLASGOW_HASKELL__ >= 702
import System.IO.Unsafe (unsafeDupablePerformIO)
#elif __GLASGOW_HASKELL__ >= 611
import GHC.IO (unsafeDupablePerformIO)
#else
import GHC.IOBase (unsafeDupablePerformIO)
#endif

import Data.Fixed (Pico)
#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed(MkFixed))
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Like 'E.mask', but backported to base before version 4.3.0.
--
-- Note that the restore callback is monomorphic, unlike in 'E.mask'.  This
-- could be fixed by changing the type signature, but it would require us to
-- enable the RankNTypes extension (since 'E.mask' has a rank-3 type).  The
-- 'withTransactionMode' function calls the restore callback only once, so we
-- don't need that polymorphism.
mask :: ((IO a -> IO a) -> IO b) -> IO b
#if MIN_VERSION_base(4,3,0)
mask io = E.mask $ \restore -> io restore
#else
mask io = do
    b <- E.blocked
    E.block $ io $ \m -> if b then m else E.unblock m
#endif
{-# INLINE mask #-}

#if !MIN_VERSION_base(4,5,0)
infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

toByteString :: Builder -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
toByteString x = toStrict (toLazyByteString x)
#else
toByteString x = B.concat (toChunks (toLazyByteString x))
#endif

#if MIN_VERSION_base(4,7,0)

toPico :: Integer -> Pico
toPico = MkFixed

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i

#else

toPico :: Integer -> Pico
toPico = unsafeCoerce

fromPico :: Pico -> Integer
fromPico = unsafeCoerce

#endif
