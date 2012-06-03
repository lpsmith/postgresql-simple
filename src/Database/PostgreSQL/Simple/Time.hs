------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Time types that supports positive and negative infinity.   Also includes 
-- new time parsers and printers with better performance than GHC's time package.
-- The parsers only understand the specific variant of ISO 8601 that PostgreSQL 
-- emits,  and the printers attempt to duplicate this syntax.  These likely have
-- problems and shortcomings.  Some that I know of:
--
-- 1. Timestamps with time zones before @1883-Nov-18 12:00:00-05@, the moment
--    Standard Railway Time went live,  cannot be parsed.  This is because
--    PostgreSQL will emit @1883-11-18 12:03:57-04:56:02@ instead of
--    @1883-11-18 11:59:59-05@,  and the timezone parser is incomplete.
--    Timestamps without time zones do not have this problem.
--
-- 2. Dates an times surrounding @1582-Feb-24@,  the date the Gregorian 
--    Calendar was introduced,  should be investigated for conversion errors.
--
-- 3. Points in time Before Christ are not also not supported.  For example,
--    PostgreSQL will emit @0045-01-01 BC@ for a value of a @date@ type.
--    This is the year that the Julian Calendar was adopted.
--
-- However, it should be noted that the old parsers also had these issues.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Time
     ( Unbounded(..)
     , Date
     , UTCTimestamp
     , ZonedTimestamp
     , LocalTimestamp
     , parseDay
     , parseUTCTime
     , parseZonedTime
     , parseLocalTime
     , parseTimeOfDay
     , parseDate
     , parseUTCTimestamp
     , parseZonedTimestamp
     , parseLocalTimestamp
     , dayToBuilder
     , utcTimeToBuilder
     , zonedTimeToBuilder
     , localTimeToBuilder
     , timeOfDayToBuilder
     , timeZoneToBuilder
     , dateToBuilder
     , utcTimestampToBuilder
     , zonedTimestampToBuilder
     , localTimestampToBuilder
     , unboundedToBuilder
     ) where

import Database.PostgreSQL.Simple.Time.Implementation
