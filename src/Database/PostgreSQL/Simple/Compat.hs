{-# LANGUAGE CPP #-}
-- | This is a module of its own, partly because it uses the CPP extension,
-- which doesn't play well with backslash-broken string literals.
module Database.PostgreSQL.Simple.Compat
    ( mask
    , (<>)
    ) where

import qualified Control.Exception as E
import Data.Monoid

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
