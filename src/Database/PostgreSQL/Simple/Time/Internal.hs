------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time.Internal
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Time parsers with improved performance over GHC's time package.  They
-- only understand the specific variant of ISO 8601 that PostgreSQL emits.
-- These parsers likely have problems and shortcomings.  Some that I know of:
--
-- 1. Timestamps with time zones before 1883-Nov-18 are not supported, due to
--    incomplete time zone parsing.
--
-- 2. Time Before Christ is not also not supported.
--
------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Simple.Time.Internal
     ( getDay
     , getDate
     , getTimeOfDay
     , getLocalTime
     , getLocalTimestamp
     , getTimeZone
     , getZonedTime
     , getZonedTimestamp
     , getUTCTime
     , getUTCTimestamp
     ) where

import Database.PostgreSQL.Simple.Time.Implementation
