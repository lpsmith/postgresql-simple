------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.HStore
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Parsers and printers for hstore,  a extended type bundled with
-- PostgreSQL providing finite maps from text strings to text strings.
-- See <https://www.postgresql.org/docs/9.5/static/hstore.html> for more
-- information.
--
-- Note that in order to use this type,  a database superuser must
-- install it by running a sql script in the share directory.  This
-- can be done on PostgreSQL 9.1 and later with the command
-- @CREATE EXTENSION hstore@.  See
-- <https://www.postgresql.org/docs/9.5/static/contrib.html> for more
-- information.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.HStore
     ( HStoreList(..)
     , HStoreMap(..)
     , ToHStore(..)
     , HStoreBuilder
     , toBuilder
     , toLazyByteString
     , hstore
     , parseHStoreList
     , ToHStoreText(..)
     , HStoreText
     ) where

import Database.PostgreSQL.Simple.HStore.Implementation
