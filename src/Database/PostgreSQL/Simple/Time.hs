{- |
Module:      Database.PostgreSQL.Simple.Time
Copyright:   (c) 2012-2015 Leon P Smith
License:     BSD3
Maintainer:  Leon P Smith <leon@melding-monads.com>
Stability:   experimental

This module provides time types that supports positive and negative
infinity,  as well as some functions for converting to and from strings.

Also, this module also contains commentary regarding postgresql's timestamp
types,  civil timekeeping in general,  and how it relates to
postgresql-simple. You can read more about PostgreSQL's date and time types
at <https://www.postgresql.org/docs/9.5/static/datatype-datetime.html>,
and the IANA time zone database at <https://en.wikipedia.org/wiki/Tz_database>.

Stack Overflow also has some excellent commentary on time,  if it is a
wiki page or a highly upvoted question and answer.   If the answer regarding
time has not received about a hundred upvotes at least,  then the answer is
almost invariably completely and painfully wrong,  even if it's the chosen
answer or the most highly upvoted answer to a question.

PostgreSQL's @timestamp with time zone@ (hereafter, @timestamptz@) can be
converted to Haskell's 'Data.Time.UTCTime' and 'Data.Time.ZonedTime' types,
because values of these types represent a self-contained, unambiguous point
in time.  PostgreSQL's @timestamp without time zone@ (hereafter, @timestamp@)
can be converted to Haskell's 'Data.Time.LocalTime',  because values of these
types are ambiguous by themselves,  and require context to disambiguate.

While this behavior may be superficially counterintuitive because the
names might suggest otherwise,  this behavior is correct.   In fact,
the \"timezone\" language in both the postgresql and haskell types would
be better read as \"offset (from UTC)\",   thus we have postgresql's
\"timestamp with offset\"  corresponding to Haskell's \"time with the
offset \'zero\'\"  and Haskell's \"time with an offset (that might be
nonzero)\".  Similarly,  postgresql's \"timestamp without an offset\"
corresponds to Haskell's \"local time (without an offset)\".

It's important to distinguish between an offset, a standard time, and
a time zone.  An offset is simply a difference of a local time from UTC,
such as @+00@, @-05@, or @+05:30@.   A standard time specifies an offset
(which may vary throughout the year, due to daylight savings) that a
region follows,  such as Universal Coordinated Time (UTC), Eastern Standard
Time\/Eastern Daylight Time (EST\/EDT), or India Standard Time (IST).
And a time zone, much like a standard time, is a function from
timestamps to offsets.

A time zone is different from a standard time because different regions
inside a standard time can be governed by different civil authorities with
different laws and thus have different histories of civil time.  An IANA
time zone is any region of the world that has had the same history of
civil time since @1970-01-01 00:00+00@.

For example, as of today,  both @America\/New_York@ and
@America\/Indiana\/Indianapolis@ are on the EST\/EDT time standard,  but
Indiana used to be on Central Standard Time until 1942, and did not observe
daylight savings time (EST only) until 2006.   Thus,  the choice between
these two time zones still matters if you are dealing with timestamps
prior to 2006,  and could become relevant again if (most of) Indiana
moves back to Central Time.  (Of course,  if the Central to Eastern switch
was the only difference,  then these two time zones would be the same in
IANA's eyes,  due to their cutoff date of 1970-01-01.)

Getting back to practicalities,  PostgreSQL's @timestamptz@ type does not
actually store an offset;  rather, it uses the offset provided to calculate
UTC, and stores the timestamp as UTC.   If an offset is not provided, the
given timestamp is assumed to be a local time for whatever the @timezone@
variable is set to, and the IANA TZ database is consulted to calculate an
offset from UTC for the time in question.

Note that while most (local timestamp, time zone) pairs correspond to exactly
one UTC timestamp, some correspond to two UTC timestamps,  while others
correspond to none at all.   The ambiguous case occurs when the civil time
is rolled back,  making a calendar day longer than 24 hours.  In this case,
PostgreSQL silently chooses the second, later possibility.  The inconsistent
case occurs when the civil time is moved forward,  making a calendar day less
than 24 hours.  In this case,  PostgreSQL silently assumes the local time
was read off a clock that had not been moved forward at the prescribed time,
and moves the clock forward for you.   Thus,  converting from local time
to UTC need not be monotonic,  if these inconsistent cases are allowed.

When retrieving a @timestamptz@,  the backend looks at the @time zone@
connection variable and then consults the IANA TZ database to calculate
an offset for the timestamp in the given time zone.

Note that while some of the information contained in the IANA TZ database
is a bit of a standardized fiction, the conversion from UTC time to a
(local time, offset) pair in a particular time zone is always unambiguous,
and the result can always be unambiguously converted back to UTC.  Thus,
postgresql-simple can interpret such a result as a 'Data.Time.ZonedTime',
or use the offset to convert back to 'Data.Time.UTCTime'.

By contrast, the @timestamp@ type ignores any offsets provided to it,
and never sends back an offset.   Thus,  postgresql-simple equates this
with 'Data.Time.LocalTime',  which has no concept of an offset.  One can
convert between @timestamptz@ and @timestamp@ using the @AT TIME ZONE@
operator, whose semantics also demonstrates that @timestamptz@ is
'Data.Time.UTCTime' whereas @timestamp@ is 'Data.Time.LocalTime'.

PostgreSQL's @timezone@ is a per-connection variable that by default is
initialized to @\'localtime\'@,  which normally corresponds to the server's
time zone.  However, this default can be modified on the server side for an
entire cluster, or on a per-user or per-database basis.  Moreover, a client
can modify their instance of the variable at any time,  and can apply that
change to the remaining duration of the connection, the current transaction,
or the execution context of a server-side function.  In addition, upon
connection initialization, the libpq client checks for the existence of
the @PGTZ@  environment variable, and if it exists, modifies @timezone@
accordingly.

With a few caveats,  postgresql-simple is designed so that you can both send
and receive timestamps with the server and get a correct result,  no matter
what the @timezone@ setting is.  But it is important to understand the caveats:

1. The correctness of server-side computations can depend on the @timezone@
   setting.  Examples include adding an @interval@ to a @timestamptz@, or
   type casting between @timestamp@ and @timestamptz@,  or applying
   the @DATE@ function to a @timestamptz@.

2. The (localtime, offset) pair contained in a 'Data.Time.ZonedTime' result
   will depend on the @timezone@ setting,  although the result will always
   represent the same instant in time regardless of the time zone.

3. Sending a 'Data.Time.LocalTime' and interpreting it as a @timestamptz@
   can be useful,  as it will be converted to UTC via the tz database,
   but correctness will depend on the @timezone@ setting.   You may prefer
   to use an explicit @AT TIME ZONE@ conversion instead, which would avoid
   this contextual dependence.

Furthermore,  although these following points don't involve the @timezone@
setting, they are related to the last point above:

1. Sending a 'Data.Time.UTCTime' and interpreting it as a @timestamp@ can
   be useful.  In practice,  the most common context used to disambiguate
   @timestamp@ is that it represents UTC,  and this coding technique will
   work as expected in this situation.

2. Sending a 'Data.Time.ZonedTime' and interpreting it as a @timestamp@ is
   almost always the wrong thing to do,  as the offset will be ignored and
   discarded.  This is likely to lead to inconsistencies in the database,
   and may lead to partial data loss.

When dealing with local timestamps that refer to the future,  it is often
useful to store it as a local time in a @timestamp@ column and store the
time zone in a second column.  One reason to do this is so that you can
convert to UTC on the fly as needed, and be protected against future changes
to the TZ database due to changes in local time standards.  In any case,
'Data.Time.ZonedTime' is not suitable for this application, because despite
its name,  it represents an offset and not a time zone.  Time zones can change;
offsets do not.  In reality,  we can't convert a local timestamp that occurs
sufficiently far in the future to UTC, because we don't know how to do it yet.

There are a few limitations and caveats that one might need to be aware
of with the current implementation when dealing with older timestamps:

For sufficiently old timestamps in almost all time zones,  the IANA TZ
database specifies offsets from UTC that is not an integral number of
minutes.   This corresponds to local mean time;  that is, astronomical
time in the city that defines the time zone.  Different time zones moved
away from local mean time to a standard time at different points in
history,  so \"sufficiently old\" depends on the time zone in question.

Thus, when retrieving a @timestamptz@ postgresql will in some cases
provide seconds in the offset.  For example:

@
$ psql
psql (9.4.5)
Type \"help\" for help.

lpsmith=> SET timezone TO \'America/New_York\';
SET
lpsmith=> VALUES (\'1883-11-18 16:59:59+00\'::timestamptz),
                 (\'1883-11-18 17:00:00+00\'::timestamptz);
           column1
------------------------------
 1883-11-18 12:03:57-04:56:02
 1883-11-18 12:00:00-05
(2 rows)
@

Both of these timestamps can be parsed as a 'Data.Time.UTCTime' type,
however 'Data.Time.ZonedTime' will fail on the former timestamp.
Because 'Data.Time.ZonedTime' assumes that offsets are an integer number
of minutes,  there isn't an particularly good solution here.

PostgreSQL,  like most software,  uses the proleptic Gregorian calendar
for its date calculations,  extending the Gregorian calendar backwards
in time before its introduction and pretending that the Julian calendar
does not exist.  For most purposes, the adoption of the Gregorian calendar
ranges from @1582-10-15@ to @1923-03-01@,  depending on location and
sometimes even political allegiances within a single location.

Timestamps BCE are not supported.  For example, PostgreSQL
will emit \"@0045-01-01 BC@\" for the first proleptic Gregorian day of
the year the Roman Empire adopted the Julian Calendar,  but
postgresql-simple does not (yet?) have the ability to either parse or
generate this syntax.   Unfortunately this syntax isn't convenient to
print or especially parse.

Also, postgresql itself cannot parse or print dates before @4714-11-24 BC@,
which is the Julian date on the proleptic Gregorian Calendar.   Although
postgresql's timestamp types are perfectly capable of representing timestamps
nearly 300,000 years in the past,  using this would require postgresql-simple
and other client programs to support binary parameters and results.

Dealing with years BCE is also complicated slightly by the fact that
Haskell's time library has a year \"0000\",  which is a convention often
used by astronomers,  while postgresql adopts the more historically
accurate convention that there is no year zero, but rather \"1 BCE\"
was immediately followed by \"1 CE\".

-}

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
     , nominalDiffTimeToBuilder
     ) where

import Database.PostgreSQL.Simple.Time.Implementation
