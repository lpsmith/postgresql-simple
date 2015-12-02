------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.HStore.Internal
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.HStore.Internal
     ( HStoreBuilder(..)
     , HStoreText(..)
     , parseHStore
     , parseHStoreKeyVal
     , parseHStoreText
     ) where

import Database.PostgreSQL.Simple.HStore.Implementation
