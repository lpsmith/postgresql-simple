------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Time
-- Copyright:   (c) 2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Time types that supports positive and negative infinity.   Also includes
-- new time parsers and printers with better performance than GHC's time
-- package.
--
-- The parsers only understand the specific variant of ISO 8601 that
-- PostgreSQL emits,  and the printers attempt to duplicate this syntax.
-- Thus the @datestyle@ parameter for the connection must be set to @ISO@.
--
-- These parsers and printers likely have problems and shortcomings.  Some
-- that I know of:
--
-- 1  @TimestampTZ@s before a timezone-dependent point in time cannot be
--    parsed,  because the parsers can only handle timezone offsets of a
--    integer number of minutes.  However, PostgreSQL will include seconds
--    in the offset, depending on the historical time standards for the city
--    identifying the time zone.
--
--    This boundary point often marks an event of some interest.  In the US
--    for example,  @timestamptz@s before @1883-Nov-18 12:00:00@ local time
--    cannot be parsed.  This is the moment Standard Railway Time went live.
--    Concretely, PostgreSQL will emit @1883-11-18 12:03:57-04:56:02@
--    instead of @1883-11-18 11:59:59-05@ when the @timezone@ parameter
--    for the connection is set to @America/New_York@.
--
-- 2. Dates and times surrounding @1582-Feb-24@,  the date the Gregorian
--    Calendar was introduced,  should be investigated for conversion errors.
--
-- 3. Points in time Before Christ are not also not supported.  For example,
--    PostgreSQL will emit @0045-01-01 BC@ for a value of a @date@ type.
--    This is the year that the Julian Calendar was adopted.
--
-- However, it should be noted that the old parsers also had issues 1 and 3.
-- Also, the new parsers now correctly handle time zones that include minutes
-- in their offset.  Most notably, this includes all of India and parts of
-- Canada and Australia.
--
-- PostgreSQL uses the zoneinfo database for its time zone information.
-- You can read more about PostgreSQL's date and time types at
-- <http://www.postgresql.org/docs/9.1/static/datatype-datetime.html>,
-- and zoneinfo at <http://en.wikipedia.org/wiki/Tz_database>.
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
