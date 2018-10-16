### Version 0.6 (2018-09-25)

  * *Breaking change*: Use `Only` package's `Only for a common 1-tuple.

    Consider a downstream library depending already both on
    `Only` and `postgresql-simple` package. This library my define
    a `MyClass` with instances for `Only.Only` and `PostgreSQL.Only`.
    As now these types are the same, a library would break.
    Therefore I consider "merging" types a breaking change.

    There are two ways for adopting this change in that scenario:

    - Either CPP-guard `PostgreSQL.Only` instance with

      ```haskell
      #if !MIN_VERSION_postgresql_simple(0,6,0)
      instance MyClass (PostgreSQL.Only a) where ...
      #endif
      ```

    - or simply remove it and add `postgresql-simple >=0.6` lower bound,
      making sure that there's only single `Only`.

  * Add `ToField` instances for case-insensitive strict and lazy text.
    Thanks to Max Tagher for the implementation.
    https://github.com/lpsmith/postgresql-simple/pull/232

  * Add support to CockroachDB.
    Thanks to Georte Steel.
    https://github.com/lpsmith/postgresql-simple/pull/245

  * Add Generic ConnectInfo instance
    Thanks to Dmitry Dzhus.
    https://github.com/lpsmith/postgresql-simple/pull/235

  * Add `fromFieldRange :: Typeable a => FieldParser a -> FieldParser (PGRange a)`
    https://github.com/lpsmith/postgresql-simple/pull/221

  * Add `fromFieldJSONByteString :: FieldParser ByteString`
    https://github.com/lpsmith/postgresql-simple/pull/222/files

  * Fix off-by-one error in year builder.
    Thanks to Nathan Ferris Hunter.
    https://github.com/lpsmith/postgresql-simple/pull/230

  * Extend ToRow and FromRow to tuples of size 18
    Thanks to Bardur Arantsson.
    https://github.com/lpsmith/postgresql-simple/pull/229

  * Add `Vector` and `Vector.Unboxed` `query` variants.
    These are more memory efficient
    (especially, if you anyway will convert to some vector)
    https://github.com/phadej/1

  * Documentation improvements
    https://github.com/lpsmith/postgresql-simple/pull/227
    https://github.com/lpsmith/postgresql-simple/pull/236

### Version 0.5.4.0 (2018-05-23)
  * Support GHC-8.4 (Semigroup/Monoid)

### Version 0.5.3.0 (2017-05-15)
  * Refactored some rudimentary cursor handling code out of the
    implementation of the fold operators,  into a new
    `Database.PostgreSQL.Simple.Cursor` module,  thanks to Bardur Arantsson.

  * Made the `FromField` instance for `Char` compatible with
    postgresql's `bpchar` type.  Thanks to Ivan Lazar Miljenovic for
    reporting the issue.

  * Added `Show` and `Eq` instances for `Notification`, thanks to
    Matvey Aksenov.

  * Fixed some example code, thanks to Matvey Aksenov.

  * Fixed a problem with using `conversionError` to throw exceptions
    of type `SomeException`.  Previously, the exception would be
    wrapped in a second `SomeException` dynamic constructor which
    would cause normal GHC typecase idioms over `SomeException` to fail.

### Version 0.5.2.1 (2016-06-29)
  * Bumped the lower bound for `base` to 4.6.   Thanks to Herbert
    Valerio Riedel for reporting the issue.

  * Added an `Eq` instance for `SqlError`, thanks to Chris Allen

  * Fixed a bug where a all-caps `"NULL"` text value inside a
    postgresql array would get parsed as the SQL null value.  Thanks
    goes to Edgar Gomes and Silk for finding and fixing this mistake.

  * Modified `withTransaction` and friends to ignore `IOError`s when
    attempting to roll back the transaction.   This fixes a buggy
    interaction between `withTransaction` and async exceptions (e.g.
    `System.Timeout`) on unix platforms.  Thanks goes to Erik
    Hesselink and Silk for providing the test case that exposed this
    issue.

  * Added the `testTimeout` regression test for the problem above.

### Version 0.5.2.0 (2016-05-25)
  * Significantly improved the error reporting from
    `Copy.putCopyData`, thanks to Ben Gamari.

  * Moved the test suite to use `tasty`,  with a big thanks
    to Ben Gamari.

  * Added `FromField.optionalField`,  and updated the documentation
    of `FromField.fromJSONField`, as inspired by an email conversation
    with Ian Wagner.

  * Updated all links in the haddocks to use https,  and added a link
    to the documentation of `connectPostgreSQL`.

  * Added a truncated changelog to the source distribution.

### Version 0.5.1.3 (2016-04-30)
  * Implemented the Monad of No Return proposal, future-proofing
    postgresql-simple against future releases of GHC.

  * Fixed a rare and usually benign race condition where
    `getNotification` could end up waiting on a newly reallocated
    file descriptor index, potentially leading to deadlock if the
    descriptor does not become readable promptly.  This fix only
    applies to GHC 7.8 or later, as it depends on `threadWaitReadSTM`.

  * Tweaked the time parsers to accept times of day of the form `hh:mm`,
    omitting seconds,  following changes made to aeson.

  * Updated the documentation of the `In` type to point out a gotcha
    when using the SQL fragment `... NOT IN ?` with `In []`.  Thanks
    goes to Simon Michael and Dan Haraj for bringing this issue to
    my attention.

### Version 0.5.1.2 (2015-12-14)
  * The syntax generated for empty arrays was changed so that
    postgresql's type inference would work better,  thanks to
    Amit Levy.

  * Further revision and expansion of the new Time documentation.

### Version 0.5.1.1 (2015-12-02)
  * This is a documentation-only release

  * The documentation of the `Time` module has been completely
    rewritten, and is far longer and more informative.  It contains
    a brief overview of civil time, the semantics of postgresql's
    time types,  and their relation to Haskell's time types via
    postgresql-simple.

  * The documentation of `connectPostgreSQL` has been modified to
    mention the effects of environment variables on the connection
    string parameters.

  * The documentation of `HStore.Internal` has been unhidden from
    haddock.

  * A typo in example code was fixed courtesy of Levi Notik.

### Version 0.5.1.0 (2015-10-22)
  * Optimized the implementation of the streaming operators to avoid
    creating intermediate lists of rows,  thanks to Timo von Holtz.

  * Added default instances for `ToRow` and `FromRow` that depend on
    Generics,  thanks to Alexey Khudyakov.

  * Fixed support for bytestring-0.9 and GHC 7.4.

### Version 0.5.0.1 (2015-09-21)
  * Fixed a bug when printing a `ZonedTime` with a negative offset
    that is not a whole number of hours.

### Version 0.5.0.0 (2015-09-19)
  * Removed the deprecated `BuiltinTypes` module.

  * Modified the SQL quasiquoter so that it returns a `Query`,  not
    an overloaded string,  and so that the `OverloadedStrings` language
    extension is no longer necessary,  thanks to Mike Ledger.

  * Moved away from `blaze-builder` in favor of `bytestring-builder`.  This
    shouldn't affect very many people, but does reduce the transitive
    dependencies.

  * Rewrote the timestamp printers to use the new `Prim`
    infrastructure in `bytestring-builder`.   The new printers should
    be a fair bit faster.

  * Added support for exclusion violations to the `ConstraintViolation`
    type in the Errors module,  thanks to João Cristóvão.

  * Moved away from the `uuid` package in favor of the `uuid-types` package,
    thanks to Bardur Arantsson.  This shouldn't affect anybody, but does
    reduce the transitive dependencies.

  * Postgresql-simple now explicitly assumes the UTF8 character encoding
    for communication between the client and server.   All database encodings
    support UTF8 except for Mule Internal Code,  the Multilingual
    Extensions for Emacs.   An exception should be raised upon
    connecting to a database by the backend if the backend cannot
    accomodate this requirement.

  * Added `Eq` and `Typeable` instances for `Connection`.

  * Added the `foldWith`, `forEachWith`, and `returningWith` families
    of functions courtesy of Travis Staton.

  * Support for Ranged types,  with thanks to Leonid Onokhov for his
    contributions.

  * The `FromField` instance for JSON now allows for top-level values that
    are not objects or arrays,  thanks to Sam Rijs.

  * The timestamp parsers have been replaced with those now in Aeson.
    Janne Hellsten adapted the old parsers from postgresql-simple for
    inclusion in Aeson;  Bryan O'Sullivan rewrote those parsers to be
    faster,  with some tweaks contributed by myself.   And now to
    bring the effort full circle,  the result has been brought back to
    postgresql-simple,  with some adaptations.

  * Fixed a bug in the typeinfo system where postgresql's `_record` type
    was being reported as a basic type and not an array type.   Thanks to
    Nickolay Kolev for helping to expose this issue.

  * Fixed a bug with the `typeInfo` operator, thanks to Timmy Tofu.  In the
    case of parsing subfields of arrays and composites,  it would fetch the
    `TypeInfo` of the array or composite type and not the subtype.

### Version 0.4.10.0 (2015-02-26)
  * Added a blurb about SSL/TLS in the documentation for connectPostgreSQL

  * Moved some functions into the Internal module,  courtesy of Aleksey
    Uimanov.

### Version 0.4.9.0 (2014-12-27)
  * Made the fromField method for PGArray available as pgArrayFieldParser,
    outside of the typeclass courtesy of Tom Ellis.

  * Fixed a missing OverloadedStrings pragma in the documentation of SqlQQ.

  * Fixed deprecation warnings, courtesy of Simon Hengel.

### Version 0.4.8.0 (2014-11-24)
  * Added support for postgresql's citext type extension via the
    case-insensitive package.

  * Added the function parseHStoreList to the HStore module.

### Version 0.4.7.0 (2014-10-27)
  * Added support for very old timestamps to UTCTime.   Depending on time
    zone,  very old timestamps can be returned with a offset from UTC that
    contains seconds.

    All timezones in the TZ database moved to a time standard offset an
    integer number of minutes from UTC before 1973, almost all locations
    moved before 1938,  and a solid majority moved before 1921.

    ZonedTime assumes offsets are a whole number of minutes,  and thus the
    conversion to ZonedTime does not support these timestamps and will
    still throw a conversion error.

    Note that PostgreSQL's "timestamp with time zone" (or "timestamptz")
    type is nearly universally misunderstood.  For an explanation, see:

        https://github.com/lpsmith/postgresql-simple/issues/69

    Thanks to Michael Snoyman for his assistance with this issue.

### Version 0.4.6.0 (2014-10-07)
  * Added an instance ToField NominalDiffTime.

### Version 0.4.5.0 (2014-09-26)
  * Added support for retrieving NaN and ±Infinity floating point values
    from postgres to the FromField instances for Float, Double, and Rational.
    The instance for Scientific is unchanged due to the fact it has no
    way of representing these special values.  Thanks goes to Tom Nielsen
    for reporting the issue.

### Version 0.4.4.1 (2014-09-07)
  * Fixed a rather serious bug that prevented the COPY module from working
    at all on unix clients since version 0.4.3.0.   Thanks goes to
    Dmitry Dzhus for reporting the issue.

  * Added a regression test for the COPY module to the test suite.

### Version 0.4.4.0 (2014-08-26)
  * Added the jsonb type debuting in PostgreSQL 9.4 to the TypeInfo.Static
    and Builtin tables, and extended the out-of-box json instances to
    be compatible with the new type.   Thanks to Tobias Florek for the
    patch.

  * Ported some expanded documentation from mysql-simple,  and fixed
    a documentation typo.

### Version 0.4.3.0 (2014-07-10)
  * connect and exec now use libpq asynchronously on non-Windows platforms.
    This means we are using threadWaitRead and threadWaitWrite to
    have GHC's IO manager schedule non-blocking C calls to libpq,
    instead of using blocking C calls and having the OS kernel do the
    scheduling.  Among other things, this now means that System.Timeout
    will work with connect and exec on unix platforms.

  * connect and exec now throw IOErrors instead of SQLErrors in
    some cases.   The intention is for SQLErrors to represent
    an error returned by the server,  and to use IOErrors for errors
    that originate from client-side code.    However,  this goal
    isn't perfectly achieved as of yet.

### Version 0.4.2.3 (2014-06-04)
  * This is strictly a documentation release,  with no code changes.

  * Fixed several documentation typos,  thanks to Chris Allen and remdezx.

  * Expanded the documentation of connectPostgreSQL,  including a short
    overview of common use cases and two new links to the official Postgres
    documentation about the authentication process.

  * De-emphasized connect and ConnectInfo in favor of connectPostgreSQL.

### Version 0.4.2.2 (2014-05-15)
  * Fixed compatibility with scientific-0.3.*,  thanks to Adam Bergmark

  * Improved documentation of the FromField module, as well as the fold,
    foldWithOptions, executeMany,  and returning operators.

### Version 0.4.2.1 (2014-03-27)
  * Fixed bug in Values syntax generation

  * Improved documentation,  including examples of multi-row update,
    a better example for Values,  documenting the inaccuracies in reading
    floating point numbers from the database,  and the IsString instance
    for QualifiedIdentifier.

### Version 0.4.2.0 (2014-03-22)
  * Added ToField and FromField instances for the scientific package

  * Changed the Identifier and QualifiedIdentifier to use Text in
    order to avoid encoding errors.  Technically this requires a
    major verson bump, but let's pretend 0.4.1.0 didn't happen.

  * Removed non-exhaustive cases in the ToField instance for Values,
    and tweaked error messages.

### Version 0.4.1.0 (2014-03-22)
  * Fixed the parsing of arrays containing null values, courtesy of
    Francesco Mazzoli

  * Added support for properly escaped identifiers,  courtesy of
    Tobias Florek.   See the new Identifier and QualifiedIdentifier
    types inside Database.PostgreSQL.Simple.Types.

  * Added support for parameterized VALUES expressions.   This is
    more general than executeMany and returning.   See the
    Database.PostgreSQL.Simple.Types.Values data type.

### Version 0.4.0.2 (2014-01-12)
  * Tweaked C preprocessor directive to be more portable

  * Tweaked testsuite for compatibility with aeson-0.7

### Version 0.4.0.1 (2013-12-21)
  * Relaxed dependency on aeson to >= 0.6

  * Update the documentation of `fromField`

### Version 0.4.0.0 (2013-12-21)
  * Changed the calling code of `fromField` so that it always sends
    a copy of the raw data.  This should be a small but significant
    performance bump for most people most of the time;  however it
    may slow down retrieval of large values not converted directly
    to ByteString,  such as large json, hstore, and array values.
    See commit 8635f8 for more information.

  * Added the PGArray type.  Thanks to Joey Adams for the suggestion

  * Fixed JSON decoding,  which was almost entirely broken up until now,
    due to bugs in the version of aeson currently available on Hackage.
    Thanks to Amit Levy for the report.

  * Added FromField instances for IORef, MVar, and IOVector.

### Version 0.3.10.0 (2013-12-17)
  * Added the queryWith function, courtesy of Leonid Onokhov

  * Added the Default type,  for representing postgresql's default values

### Version 0.3.9.1 (2013-10-28)
  * Removed dependency on hashable

### Version 0.3.9.0 (2013-10-27)
  * Added FromField and ToField instances for the `uuid` package,
    courtesy of Bas van Dijk.

  * Added instance FromRow (Maybe a) for most pre-packaged FromRow
    instances.  See issue #64 for some discussion.

  * Added the fromBinary, fromHStoreMap, and fromHStoreList newtype
    unwrapper functions, courtesy of Bas van Dijk.

### Version 0.3.8.0 (2013-10-11)
  * Fixed the example code in `FromField`, thanks to Adam Bergmark.

  * Added `Notification.getBackendPID`.

### Version 0.3.7.1 (2013-09-12)
  * Relaxed the dependency on bytestring-0.10 back to bytestring-0.9,
    courtesy of Michael Snoyman

### Version 0.3.7.0 (2013-09-11)
  * Added `aeson` as a dependency.

  * Added ToField and FromField instances for aeson's JSON.Value type,
    courtesy of Bas van Dijk.

  * Added toJSONField and fromJSONField helper functions for declaring
    FromField/ToField JSON instances to other Haskell types, courtesy
    of Bas van Dijk.

  * Added a FromField instance for (), corresponding to postgresql's void
    type.

  * Added liftConversion and liftRowParser functions to the Internal
    module, for lifting IO actions into the respective monads.

  * The SqlError predicates available in the Transaction module are now
    also exported from the Errors module.

  * Various documentation fixes.

### Version 0.3.6.0 (2013-08-19)
  * Added the json type to BuiltinTypes and TypeInfo.Static, courtesy of
    Manuel Gómez.

  * Removed the remaining internal dependencies on BuiltinTypes from
    FromField.   Added the TypeInfo.Macro module as a result.

  * Deprecated the BuiltinTypes module,  which will be removed several
    months from now.  Fixed the example code in FromField to reflect
    this change.

### Version 0.3.5.0 (2013-08-09)
  * Added an FromRow instance for Vector,  semantically identical to the
    existing FromRow instance for [],  courtesy of Doug Beardsley

  * Reworked the documentation for the Copy module,  and tweaked the
    documentation for the LargeObjects module.

### Version 0.3.4.0 (2013-07-23)
  * Added direct support for COPY IN and COPY OUT,  without having
    to use raw postgresql-libpq calls and postgresql-simple's Internal
    module.

  * Changed `getNotification` so that it throws a IOError (resource vanished)
    exception instead of an ErrorCall exception when it fails to fetch
    the connection's file descriptor from libpq.

### Version 0.3.3.2 (2013-06-18)
  * Optimized the definition of `mconcat` in the Monoid instance for
    the Query type,  courtesy of Joey Adams.

### Version 0.3.3.1 (2013-06-06)
  * `getNotification` now works on Windows,  albeit using a one-second
    polling loop,  courtesy of Joey Adams.

### Version 0.3.3.0 (2013-05-29)
  * Fixed two issues with the fold operator:  fold would raise the wrong
    exception,  and gave the database cursor a static name preventing
    folds from being nested.  Thanks to Joey Adams for his
    work on these issues.

### Version 0.3.2.0 (2013-05-20)
  * Added a savepoint abstraction to the Transaction module, courtesy
    of Joey Adams

### Version 0.3.1.2 (2013-04-29)
  * Fixed hstore parser to not unnecessarily reverse the key-value pairs

### Version 0.3.1.1 (2013-04-29)
  * Fixed hstore parser to recognize empty hstores, courtesy of Simon
    Meier

### Version 0.3.1.0 (2013-04-26)
  * Added support for Range and Composite types to the TypeInfo system.

  * Added support for hstore types in the Simple.HStore module.

  * Improved documentation of the FromField module.

### Version 0.3.0.1 (2013-03-26)
  * A large chunk of the documentation inside the FromField module had
    silently failed to render in Haddock.

### Version 0.3.0.0 (2013-03-25)
  * Added support for PostgreSQL's Array Types.  Thanks to Jason Dusek
    for his work on this feature.

  * Added a brand new TypeInfo system that gives FromField instances
    convenient and efficient access to the pg_type metatable.   This
    replaced the older typename cache,  and was neccesary to properly
    support postgres array types.   Thanks to Bas van Dijk for his
    work on this feature.

  * Changed the type of the `fromField` and `fromRow` methods to allow
    a restricted set of IO actions,  and stopped pre-calculating the type
    name of every column.    As a result,  the type of the `typename`
    operator changed from `Field -> ByteString` to
    `Field -> Conversion ByteString`,  where Conversion is the new monad
    that conversion computations run inside.

  * Improved the documentation of the FromField module.

  * Added the Database.PostgreSQL.Simple.Errors module,  which offers
    some predicates and functions for interpreting SqlError values, courtesy
    of Leonid Onokhov.

  * Added a the name of a column and the associated table's object identifier
    to ResultError exceptions,  courtesy of Jeff Chu.

  * Moved most of the more detailed transaction operators into the
    Database.PostgreSQL.Simple.Transaction module.

  * Changed withTransactionModeRetry to accept a predicate of which
    SqlErrors to retry,  due to the fact that serialization errors can
    sometimes manifest themselves as constraint violations.   Thanks
    to Oliver Charles for pointing this out and implementing the change.

  * Added simple tests of the fold operator,  thanks to Joey Adams.

  * Added simple tests of the array conversion code.

  * Added recognition of -- comments in the quasiquoter,  which are
    now stripped out.


### Version 0.2.4.1 (2012-08-29)
  * Fixed the documentation of `In`.   Thanks to rekado and dstcruz for
    pointing this out.

### Version 0.2.4.0 (2012-08-23)
  * Added the `withTransactionSerializable` and `withTransactionModeRetry`
    operators,  thanks to Joey Adams.

### Version 0.2.3.0 (2012-08-09)
  * Added the `returning` operator, thanks to Jason Dusek

### Version 0.2.2.0 (2012-07-26)
  * Added a ToRow instance for the (:.) type, courtesy of Leonid Onokhov

  * Added the type oid for PostgreSQL's `uuid` type to BuiltinTypes

### Version 0.2.1.0 (2012-07-23)
  * Added the FromRow.fieldWith operator, thanks to Leonid Onokhov

  * Added a type synonym for FieldParser

### Version 0.2.0.1 (2012-06-21)
  * Fixed a compatibility problem with PostgreSQL 8.1,  which does not allow
    clients to set their own value for `standard_conforming_strings`.  This
    connection variable is still set to `on` for PostgreSQL 8.2 and later.

### Version 0.2: (2012-06-19)
  * Removed the conversion from `timestamp` to `UTCTime`.  Some code will be
    broken even though it will still compile.

  * Renamed a number of data constructors, mostly in the BuiltinTypes module.

  * Exported ToRow/FromRow from Database.PostgreSQL.Simple


### Version 0.1.4.3: (2012-06-10)
  * Fix language extensions for compatibility with GHC 7.0

### Version 0.1.4.2: (2012-06-10)
  * Fix a wayward dependency on Text.

### Version 0.1.4.1: (2012-06-10)
  * Added support for timezones with minutes in their UTC offset.

### Version 0.1.4: (2012-06-10)
  * Removed pcre-light dependency,  courtesy of Joey Adams.

  * Reworked support for the Time types.

      * The conversion from PostgreSQL's `timestamp` (without time zone) type
        to Haskell's `UTCTime` type is deprecated and will be removed in 0.2.

      * `Data.Time.LocalTime` now has `FromField`/`ToField` instances.  It is
        now the preferred way of dealing with `timestamp` (without time zone).

      * `Database.PostgreSQL.Simple.Time` is a new module that offers types
        that accomodate PostgreSQL's infinities.

      * All time-related `FromField`/`ToField` instances are now based on new,
        higher-speed parsers and printers instead of those provided by the
        time package included in GHC.

  * Planned breaking changes for 0.2:

      * Removing the conversion from `timestamp` to `UTCTime`.

      * Renaming some of the type names in `BuiltinTypes`.


### Version 0.1.3: (2012-05-30)
  * Made ZonedTime an instance of FromField and ToField

  * Added getNotificationNonBlocking


### Version 0.1.2: (2012-05-09)
  * Switched to libpq-based escaping for bytea types;  Binary now works with
    PostgreSQL 8 courtesy of Joey Adams.

  * postgresql-simple now sets standard_conforming_strings to "on".  This
    per-connection variable is initialized according to the server
    configuration,  which defaults to "off" for PostgreSQL < 9,  and "on"
    for PostgreSQL >= 9.   You may need to adjust any string literals in
    your SQL queries,  or set the variable yourself.

  * Exported (:.) from Database.PostgreSQL.Simple


### Version 0.1.1: (2012-05-06)
  * Added some preliminary documentation for the Ok, Notification, and
    LargeObjects modules

  * Implemented the `fail` method for the monad instance for `Ok`.

  * Fixed a bug relating to handling the transaction level


### Version 0.1:   (2012-05-04)
  * Renamed several modules, typeclasses, and functions:

        QueryParams  (renderParams)   -> ToRow   (toRow)
        QueryResults (convertResults) -> FromRow (fromRow)
        Param  (render)  -> ToField   (toField)
        Result (convert) -> FromField (fromField)

  * Added the `Database.PostgreSQL.Simple.Ok` module,  a variation of
    `Either SomeException` that has an instance for `Alternative` and also
    uses a list of exceptions to track the ways it has failed.

  * Changed the return type of `fromField` and `fromRow` from
    `Either SomeException` to `Ok`.

  * Thanks to suggestions from Ozgun Ataman, the `FromRow` typeclass has been
    massively improved.  The result is simpler definitions and better
    compositionality.  Also, user-defined instances need not be to be
    concerned about forcing the converted results to WHNF.  Here is an
    example comparing the old to the new:

        instance (Result a, Result b) => QueryResults (a,b) where
            convertResults [fa,fb] [va,vb] = do
                !a <- convert fa va
                !b <- convert fb vb
                return (a,b)
            convertResults fs vs  = convertError fs vs 2

        instance (FromField a, FromField b) => FromRow (a,b) where
            fromRow = (,) <$> field <*> field

  * Added `(:.)`, a pair that allows one to compose `FromRow` instances:

        instance (FromRow a, FromRow b) => FromRow (a :. b) where
            fromRow = (:.) <$> fromRow <*> fromRow

  * Moved the contents `Field` module into the `FromField` module.

  * Removed the `RawResult` type.

  * Added `DefaultIsolationLevel` as a distinct `IsolationLevel` option
    and   `DefaultReadWriteMode`  as a distinct `ReadWriteMode`.
