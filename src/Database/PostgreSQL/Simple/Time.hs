------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Time types that supports infinities.   Also includes new time parsers
-- with improved performance over GHC's time package.  See 
-- 'Database.PostgreSQL.Simple.Time.Internal'.
--
------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Simple.Time
     ( Unbounded(..)
     , UTCTimestamp
     , parseUTCTimestamp
     , ZonedTimestamp
     , parseZonedTimestamp
     , LocalTimestamp
     , parseLocalTimestamp
     , Date
     , parseDate
     ) where

import Database.PostgreSQL.Simple.Time.Implementation
