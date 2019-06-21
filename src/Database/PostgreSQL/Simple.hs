{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- A mid-level client library for the PostgreSQL database, aimed at ease of
-- use and high performance.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple
    (
    -- * Writing queries
    -- $use

    -- ** The Query type
    -- $querytype

    -- ** Parameter substitution
    -- $subst

    -- *** Type inference
    -- $inference

    -- ** Substituting a single parameter
    -- $only_param

    -- ** Representing a list of values
    -- $in

    -- ** Modifying multiple rows at once
    -- $many

    -- ** @RETURNING@: modifications that return results
    -- $returning

    -- * Extracting results
    -- $result

    -- ** Handling null values
    -- $null

    -- ** Type conversions
    -- $types

    -- * Types
      Connection
    , Query
    , ToRow
    , FromRow
    , In(..)
    , Binary(..)
    , Only(..)
    , (:.)(..)
    -- ** Exceptions
    , SqlError(..)
    , PQ.ExecStatus(..)
    , FormatError(..)
    , QueryError(..)
    , ResultError(..)
    -- * Connection management
    , Base.connectPostgreSQL
    , Base.close
    , Base.connect
    , Base.ConnectInfo(..)
    , Base.defaultConnectInfo
    , Base.postgreSQLConnectionString
    -- * Queries that return results
    , query
    , query_
    -- ** Queries taking parser as argument
    , queryWith
    , queryWith_
    -- * Queries that stream results
    , FoldOptions(..)
    , FetchQuantity(..)
    , defaultFoldOptions
    , fold
    , foldWithOptions
    , fold_
    , foldWithOptions_
    , forEach
    , forEach_
    , returning
    -- ** Queries that stream results taking a parser as an argument
    , foldWith
    , foldWithOptionsAndParser
    , foldWith_
    , foldWithOptionsAndParser_
    , forEachWith
    , forEachWith_
    , returningWith
    -- * Statements that do not return results
    , execute
    , execute_
    , executeMany
--    , Base.insertID
    -- * Transaction handling
    , withTransaction
    , withSavepoint
--    , Base.autocommit
    , begin
    , commit
    , rollback
    -- * Helper functions
    , formatMany
    , formatQuery
    ) where

import           Data.ByteString.Builder (Builder, byteString, char8)
import           Control.Applicative ((<$>))
import           Control.Exception as E
import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import           Data.List (intersperse)
import           Data.Monoid (mconcat)
import           Database.PostgreSQL.Simple.Compat ((<>), toByteString)
import           Database.PostgreSQL.Simple.Cursor
import           Database.PostgreSQL.Simple.FromField (ResultError(..))
import           Database.PostgreSQL.Simple.FromRow (FromRow(..))
import           Database.PostgreSQL.Simple.ToField (Action(..))
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.Types
                   ( Binary(..), In(..), Only(..), Query(..), (:.)(..) )
import           Database.PostgreSQL.Simple.Internal as Base
import           Database.PostgreSQL.Simple.Internal.PQResultUtils
import           Database.PostgreSQL.Simple.Transaction
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString.Char8 as B


-- | Format a query string.
--
-- This function is exposed to help with debugging and logging. Do not
-- use it to prepare queries for execution.
--
-- String parameters are escaped according to the character set in use
-- on the 'Connection'.
--
-- Throws 'FormatError' if the query string could not be formatted
-- correctly.
formatQuery :: ToRow q => Connection -> Query -> q -> IO ByteString
formatQuery conn q@(Query template) qs
    | null xs && '?' `B.notElem` template = return template
    | otherwise = toByteString <$> buildQuery conn q template xs
  where xs = toRow qs

-- | Format a query string with a variable number of rows.
--
-- This function is exposed to help with debugging and logging. Do not
-- use it to prepare queries for execution.
--
-- The query string must contain exactly one substitution group,
-- identified by the SQL keyword \"@VALUES@\" (case insensitive)
-- followed by an \"@(@\" character, a series of one or more \"@?@\"
-- characters separated by commas, and a \"@)@\" character. White
-- space in a substitution group is permitted.
--
-- Throws 'FormatError' if the query string could not be formatted
-- correctly.
formatMany :: (ToRow q) => Connection -> Query -> [q] -> IO ByteString
formatMany _ q [] = fmtError "no rows supplied" q []
formatMany conn q@(Query template) qs = do
  case parseTemplate template of
    Just (before, qbits, after) -> do
      bs <- mapM (buildQuery conn q qbits . toRow) qs
      return . toByteString . mconcat $ byteString before :
                                        intersperse (char8 ',') bs ++
                                        [byteString after]
    Nothing -> fmtError "syntax error in multi-row template" q []

-- Split the input string into three pieces, @before@, @qbits@, and @after@,
-- following this grammar:
--
-- start: ^ before qbits after $
--     before: ([^?]* [^?\w])? 'VALUES' \s*
--     qbits:  '(' \s* '?' \s* (',' \s* '?' \s*)* ')'
--     after:  [^?]*
--
-- \s: [ \t\n\r\f]
-- \w: [A-Z] | [a-z] | [\x80-\xFF] | '_' | '$' | [0-9]
--
-- This would be much more concise with some sort of regex engine.
-- 'formatMany' used to use pcre-light instead of this hand-written parser,
-- but pcre is a hassle to install on Windows.
parseTemplate :: ByteString -> Maybe (ByteString, ByteString, ByteString)
parseTemplate template =
    -- Convert input string to uppercase, to facilitate searching.
    search $ B.map toUpper_ascii template
  where
    -- Search for the next occurrence of "VALUES"
    search bs =
        case B.breakSubstring "VALUES" bs of
            (x, y)
                -- If "VALUES" is not present in the string, or any '?' characters
                -- were encountered prior to it, fail.
                | B.null y || ('?' `B.elem` x)
               -> Nothing

                -- If "VALUES" is preceded by an identifier character (a.k.a. \w),
                -- try the next occurrence.
                | not (B.null x) && isIdent (B.last x)
               -> search $ B.drop 6 y

                -- Otherwise, we have a legitimate "VALUES" token.
                | otherwise
               -> parseQueryBits $ skipSpace $ B.drop 6 y

    -- Parse '(' \s* '?' \s* .  If this doesn't match
    -- (and we don't consume a '?'), look for another "VALUES".
    --
    -- qb points to the open paren (if present), meaning it points to the
    -- beginning of the "qbits" production described above.  This is why we
    -- pass it down to finishQueryBits.
    parseQueryBits qb
        | Just ('(', skipSpace -> bs1) <- B.uncons qb
        , Just ('?', skipSpace -> bs2) <- B.uncons bs1
        = finishQueryBits qb bs2
        | otherwise
        = search qb

    -- Parse (',' \s* '?' \s*)* ')' [^?]* .
    --
    -- Since we've already consumed at least one '?', there's no turning back.
    -- The parse has to succeed here, or the whole thing fails
    -- (because we don't allow '?' to appear outside of the VALUES list).
    finishQueryBits qb bs0
        | Just (')', bs1) <- B.uncons bs0
        = if '?' `B.elem` bs1
              then Nothing
              else Just $ slice3 template qb bs1
        | Just (',', skipSpace -> bs1) <- B.uncons bs0
        , Just ('?', skipSpace -> bs2) <- B.uncons bs1
        = finishQueryBits qb bs2
        | otherwise
        = Nothing

    -- Slice a string into three pieces, given the start offset of the second
    -- and third pieces.  Each "offset" is actually a tail of the uppercase
    -- version of the template string.  Its length is used to infer the offset.
    --
    -- It is important to note that we only slice the original template.
    -- We don't want our all-caps trick messing up the actual query string.
    slice3 source p1 p2 =
        (s1, s2, source'')
      where
        (s1, source')  = B.splitAt (B.length source - B.length p1) source
        (s2, source'') = B.splitAt (B.length p1     - B.length p2) source'

    toUpper_ascii c | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
                    | otherwise            = c

    -- Based on the definition of {ident_cont} in src/backend/parser/scan.l
    -- in the PostgreSQL source.  No need to check [a-z], since we converted
    -- the whole string to uppercase.
    isIdent c = (c >= '0'    && c <= '9')
             || (c >= 'A'    && c <= 'Z')
             || (c >= '\x80' && c <= '\xFF')
             || c == '_'
             || c == '$'

    -- Based on {space} in scan.l
    isSpace_ascii c = (c == ' ') || (c >= '\t' && c <= '\r')

    skipSpace = B.dropWhile isSpace_ascii


buildQuery :: Connection -> Query -> ByteString -> [Action] -> IO Builder
buildQuery conn q template xs =
    zipParams (split template) <$> mapM (buildAction conn q xs) xs
  where split s =
            -- This part escapes double '??'s to make literal '?'s possible
            -- in PostgreSQL queries using the JSON operators: @?@, @?|@ and @?&@
            let (h,t) = breakOnSingleQuestionMark s
            in byteString h
               : if B.null t
                 then []
                 else split (B.tail t)
        zipParams (t:ts) (p:ps) = t <> p <> zipParams ts ps
        zipParams [t] []        = t
        zipParams _ _ = fmtError (show countSingleQs ++
                                  " single '?' characters, but " ++
                                  show (length xs) ++ " parameters") q xs
        countSingleQs = go 0 template
          where go i "" = (i :: Int)
                go i bs = case qms of
                            ("?","?") -> go i nextQMBS
                            ("?",_) -> go (i+1) nextQMBS
                            _ -> i
                  where qms = B.splitAt 1 qmBS
                        (qmBS,nextQMBS) = B.splitAt 2 qmBS'
                        qmBS' = B.dropWhile (/= '?') bs

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.
--
-- Throws 'FormatError' if the query could not be formatted correctly, or
-- a 'SqlError' exception if the backend returns an error.
execute :: (ToRow q) => Connection -> Query -> q -> IO Int64
execute conn template qs = do
  result <- exec conn =<< formatQuery conn template qs
  finishExecute conn template result

-- | Execute a multi-row @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.   If the list of parameters is empty,
-- this function will simply return 0 without issuing the query to the backend.
-- If this is not desired, consider using the 'Values' constructor instead.
--
-- Throws 'FormatError' if the query could not be formatted correctly, or
-- a 'SqlError' exception if the backend returns an error.
--
-- For example,  here's a command that inserts two rows into a table
-- with two columns:
--
-- @
-- executeMany c [sql|
--     INSERT INTO sometable VALUES (?,?)
--  |] [(1, \"hello\"),(2, \"world\")]
-- @
--
-- Here's an canonical example of a multi-row update command:
--
-- @
-- executeMany c [sql|
--     UPDATE sometable
--        SET y = upd.y
--       FROM (VALUES (?,?)) as upd(x,y)
--      WHERE sometable.x = upd.x
--  |] [(1, \"hello\"),(2, \"world\")]
-- @

executeMany :: (ToRow q) => Connection -> Query -> [q] -> IO Int64
executeMany _ _ [] = return 0
executeMany conn q qs = do
  result <- exec conn =<< formatMany conn q qs
  finishExecute conn q result

-- | Execute @INSERT ... RETURNING@, @UPDATE ... RETURNING@, or other SQL
-- query that accepts multi-row input and is expected to return results.
-- Note that it is possible to write
--    @'query' conn "INSERT ... RETURNING ..." ...@
-- in cases where you are only inserting a single row,  and do not need
-- functionality analogous to 'executeMany'.
--
-- If the list of parameters is empty,  this function will simply return @[]@
-- without issuing the query to the backend.   If this is not desired,
-- consider using the 'Values' constructor instead.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
returning :: (ToRow q, FromRow r) => Connection -> Query -> [q] -> IO [r]
returning = returningWith fromRow

-- | A version of 'returning' taking parser as argument
returningWith :: (ToRow q) => RowParser r -> Connection -> Query -> [q] -> IO [r]
returningWith _ _ _ [] = return []
returningWith parser conn q qs = do
  result <- exec conn =<< formatMany conn q qs
  finishQueryWith parser conn q result

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
--
-- When processing large results, this function will consume a lot of
-- client-side memory.  Consider using 'fold' instead.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
--
-- * 'SqlError':  the postgresql backend returned an error,  e.g.
--   a syntax or type error,  or an incorrect table or column name.
query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
query = queryWith fromRow

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ = queryWith_ fromRow

-- | A version of 'query' taking parser as argument
queryWith :: ToRow q => RowParser r -> Connection -> Query -> q -> IO [r]
queryWith parser conn template qs = do
  result <- exec conn =<< formatQuery conn template qs
  finishQueryWith parser conn template result

-- | A version of 'query_' taking parser as argument
queryWith_ :: RowParser r -> Connection -> Query -> IO [r]
queryWith_ parser conn q@(Query que) = do
  result <- exec conn que
  finishQueryWith parser conn q result

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. Results are streamed incrementally from the server, and
-- consumed via a left fold.
--
-- When dealing with small results, it may be simpler (and perhaps
-- faster) to use 'query' instead.
--
-- This fold is /not/ strict. The stream consumer is responsible for
-- forcing the evaluation of its result to avoid space leaks.
--
-- This is implemented using a database cursor.    As such,  this requires
-- a transaction.   This function will detect whether or not there is a
-- transaction in progress,  and will create a 'ReadCommitted' 'ReadOnly'
-- transaction if needed.   The cursor is given a unique temporary name,
-- so the consumer may itself call fold.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
--
-- * 'SqlError':  the postgresql backend returned an error,  e.g.
--   a syntax or type error,  or an incorrect table or column name.
fold            :: ( FromRow row, ToRow params )
                => Connection
                -> Query
                -> params
                -> a
                -> (a -> row -> IO a)
                -> IO a
fold = foldWithOptions defaultFoldOptions

-- | A version of 'fold' taking a parser as an argument
foldWith        :: ( ToRow params )
                => RowParser row
                -> Connection
                -> Query
                -> params
                -> a
                -> (a -> row -> IO a)
                -> IO a
foldWith = foldWithOptionsAndParser defaultFoldOptions

-- | Number of rows to fetch at a time.   'Automatic' currently defaults
--   to 256 rows,  although it might be nice to make this more intelligent
--   based on e.g. the average size of the rows.
data FetchQuantity
   = Automatic
   | Fixed !Int

data FoldOptions
   = FoldOptions {
       fetchQuantity   :: !FetchQuantity,
       transactionMode :: !TransactionMode
     }

-- | defaults to 'Automatic',  and 'TransactionMode' 'ReadCommitted' 'ReadOnly'
defaultFoldOptions :: FoldOptions
defaultFoldOptions = FoldOptions {
      fetchQuantity   = Automatic,
      transactionMode = TransactionMode ReadCommitted ReadOnly
    }

-- | The same as 'fold',  but this provides a bit more control over
--   lower-level details.  Currently,  the number of rows fetched per
--   round-trip to the server and the transaction mode may be adjusted
--   accordingly.    If the connection is already in a transaction,
--   then the existing transaction is used and thus the 'transactionMode'
--   option is ignored.
foldWithOptions :: ( FromRow row, ToRow params )
                => FoldOptions
                -> Connection
                -> Query
                -> params
                -> a
                -> (a -> row -> IO a)
                -> IO a
foldWithOptions opts = foldWithOptionsAndParser opts fromRow

-- | A version of 'foldWithOptions' taking a parser as an argument
foldWithOptionsAndParser :: (ToRow params)
                         => FoldOptions
                         -> RowParser row
                         -> Connection
                         -> Query
                         -> params
                         -> a
                         -> (a -> row -> IO a)
                         -> IO a
foldWithOptionsAndParser opts parser conn template qs a f = do
    q <- formatQuery conn template qs
    doFold opts parser conn template (Query q) a f

-- | A version of 'fold' that does not perform query substitution.
fold_ :: (FromRow r) =>
         Connection
      -> Query                  -- ^ Query.
      -> a                      -- ^ Initial state for result consumer.
      -> (a -> r -> IO a)       -- ^ Result consumer.
      -> IO a
fold_ = foldWithOptions_ defaultFoldOptions

-- | A version of 'fold_' taking a parser as an argument
foldWith_ :: RowParser r
          -> Connection
          -> Query
          -> a
          -> (a -> r -> IO a)
          -> IO a
foldWith_ = foldWithOptionsAndParser_ defaultFoldOptions

foldWithOptions_ :: (FromRow r) =>
                    FoldOptions
                 -> Connection
                 -> Query             -- ^ Query.
                 -> a                 -- ^ Initial state for result consumer.
                 -> (a -> r -> IO a)  -- ^ Result consumer.
                 -> IO a
foldWithOptions_ opts conn query' a f = doFold opts fromRow conn query' query' a f

-- | A version of 'foldWithOptions_' taking a parser as an argument
foldWithOptionsAndParser_ :: FoldOptions
                          -> RowParser r
                          -> Connection
                          -> Query             -- ^ Query.
                          -> a                 -- ^ Initial state for result consumer.
                          -> (a -> r -> IO a)  -- ^ Result consumer.
                          -> IO a
foldWithOptionsAndParser_ opts parser conn query' a f = doFold opts parser conn query' query' a f

doFold :: FoldOptions
       -> RowParser row
       -> Connection
       -> Query
       -> Query
       -> a
       -> (a -> row -> IO a)
       -> IO a
doFold FoldOptions{..} parser conn _template q a0 f = do
    stat <- withConnection conn PQ.transactionStatus
    case stat of
      PQ.TransIdle    -> withTransactionMode transactionMode conn go
      PQ.TransInTrans -> go
      PQ.TransActive  -> fail "foldWithOpts FIXME:  PQ.TransActive"
         -- This _shouldn't_ occur in the current incarnation of
         -- the library,  as we aren't using libpq asynchronously.
         -- However,  it could occur in future incarnations of
         -- this library or if client code uses the Internal module
         -- to use raw libpq commands on postgresql-simple connections.
      PQ.TransInError -> fail "foldWithOpts FIXME:  PQ.TransInError"
         -- This should be turned into a better error message.
         -- It is probably a bad idea to automatically roll
         -- back the transaction and start another.
      PQ.TransUnknown -> fail "foldWithOpts FIXME:  PQ.TransUnknown"
         -- Not sure what this means.
  where
    declare =
      declareCursor conn q
    fetch cursor a =
      foldForwardWithParser cursor parser chunkSize f a

    go = bracket declare closeCursor $ \cursor ->
             let loop a = fetch cursor a >>=
                            \r -> case r of
                                    Left x -> return x
                                    Right x -> loop x
               in loop a0

-- FIXME: choose the Automatic chunkSize more intelligently
--   One possibility is to use the type of the results,  although this
--   still isn't a perfect solution, given that common types (e.g. text)
--   are of highly variable size.
--   A refinement of this technique is to pick this number adaptively
--   as results are read in from the database.
    chunkSize = case fetchQuantity of
                 Automatic   -> 256
                 Fixed n     -> n

-- | A version of 'fold' that does not transform a state value.
forEach :: (ToRow q, FromRow r) =>
           Connection
        -> Query                -- ^ Query template.
        -> q                    -- ^ Query parameters.
        -> (r -> IO ())         -- ^ Result consumer.
        -> IO ()
forEach = forEachWith fromRow
{-# INLINE forEach #-}

-- | A version of 'forEach' taking a parser as an argument
forEachWith :: ( ToRow q )
            => RowParser r
            -> Connection
            -> Query
            -> q
            -> (r -> IO ())
            -> IO ()
forEachWith parser conn template qs = foldWith parser conn template qs () . const
{-# INLINE forEachWith #-}

-- | A version of 'forEach' that does not perform query substitution.
forEach_ :: (FromRow r) =>
            Connection
         -> Query                -- ^ Query template.
         -> (r -> IO ())         -- ^ Result consumer.
         -> IO ()
forEach_ = forEachWith_ fromRow
{-# INLINE forEach_ #-}

forEachWith_ :: RowParser r
             -> Connection
             -> Query
             -> (r -> IO ())
             -> IO ()
forEachWith_ parser conn template = foldWith_ parser conn template () . const
{-# INLINE forEachWith_ #-}


-- $use
--
-- SQL-based applications are somewhat notorious for their
-- susceptibility to attacks through the injection of maliciously
-- crafted data. The primary reason for widespread vulnerability to
-- SQL injections is that many applications are sloppy in handling
-- user data when constructing SQL queries.
--
-- This library provides a 'Query' type and a parameter substitution
-- facility to address both ease of use and security.

-- $querytype
--
-- A 'Query' is a @newtype@-wrapped 'ByteString'. It intentionally
-- exposes a tiny API that is not compatible with the 'ByteString'
-- API; this makes it difficult to construct queries from fragments of
-- strings.  The 'query' and 'execute' functions require queries to be
-- of type 'Query'.
--
-- To most easily construct a query, enable GHC's @OverloadedStrings@
-- language extension and write your query as a normal literal string.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.PostgreSQL.Simple
-- >
-- > hello :: IO Int
-- > hello = do
-- >   conn <- connectPostgreSQL ""
-- >   [Only i] <- query_ conn "select 2 + 2"
-- >   return i
--
-- A 'Query' value does not represent the actual query that will be
-- executed, but is a template for constructing the final query.

-- $subst
--
-- Since applications need to be able to construct queries with
-- parameters that change, this library provides a query substitution
-- capability.
--
-- The 'Query' template accepted by 'query' and 'execute' can contain
-- any number of \"@?@\" characters.  Both 'query' and 'execute'
-- accept a third argument, typically a tuple. When constructing the
-- real query to execute, these functions replace the first \"@?@\" in
-- the template with the first element of the tuple, the second
-- \"@?@\" with the second element, and so on. If necessary, each
-- tuple element will be quoted and escaped prior to substitution;
-- this defeats the single most common injection vector for malicious
-- data.
--
-- For example, given the following 'Query' template:
--
-- > select * from user where first_name = ? and age > ?
--
-- And a tuple of this form:
--
-- > ("Boris" :: String, 37 :: Int)
--
-- The query to be executed will look like this after substitution:
--
-- > select * from user where first_name = 'Boris' and age > 37
--
-- If there is a mismatch between the number of \"@?@\" characters in
-- your template and the number of elements in your tuple, a
-- 'FormatError' will be thrown.
--
-- Note that the substitution functions do not attempt to parse or
-- validate your query. It's up to you to write syntactically valid
-- SQL, and to ensure that each \"@?@\" in your query template is
-- matched with the right tuple element.

-- $inference
--
-- Automated type inference means that you will often be able to avoid
-- supplying explicit type signatures for the elements of a tuple.
-- However, sometimes the compiler will not be able to infer your
-- types. Consider a case where you write a numeric literal in a
-- parameter tuple:
--
-- > query conn "select ? + ?" (40,2)
--
-- The above query will be rejected by the compiler, because it does
-- not know the specific numeric types of the literals @40@ and @2@.
-- This is easily fixed:
--
-- > query conn "select ? + ?" (40 :: Double, 2 :: Double)
--
-- The same kind of problem can arise with string literals if you have
-- the @OverloadedStrings@ language extension enabled.  Again, just
-- use an explicit type signature if this happens.
--
-- Finally, remember that the compiler must be able to infer the type
-- of a query's /results/ as well as its parameters.  We might like
-- the following example to work:
--
-- > print =<< query_ conn "select 2 + 2"
--
-- Unfortunately, while a quick glance tells us that the result type
-- should be a single row containing a single numeric column, the
-- compiler has no way to infer what the types are.  We can easily fix
-- this by providing an explicit type annotation:
--
-- > xs <- query_ conn "select 2 + 2"
-- > print (xs :: [Only Int])

-- $only_param
--
-- Haskell lacks a single-element tuple type, so if you have just one
-- value you want substituted into a query or a single-column result,
-- what should you do?
--
-- The obvious approach would appear to be something like this:
--
-- > instance (ToField a) => ToRow a where
-- >     ...
--
-- Unfortunately, this wreaks havoc with type inference, so we take a
-- different tack. To represent a single value @val@ as a parameter, write
-- a singleton list @[val]@, use 'Just' @val@, or use 'Only' @val@.
--
-- Here's an example using a singleton list:
--
-- > execute conn "insert into users (first_name) values (?)"
-- >              ["Nuala"]
--
-- A row of /n/ query results is represented using an /n/-tuple, so
-- you should use 'Only' to represent a single-column result.

-- $in
--
-- Suppose you want to write a query using an @IN@ clause:
--
-- > select * from users where first_name in ('Anna', 'Boris', 'Carla')
--
-- In such cases, it's common for both the elements and length of the
-- list after the @IN@ keyword to vary from query to query.
--
-- To address this case, use the 'In' type wrapper, and use a single
-- \"@?@\" character to represent the list.  Omit the parentheses
-- around the list; these will be added for you.
--
-- Here's an example:
--
-- > query conn "select * from users where first_name in ?" $
-- >       Only $ In ["Anna", "Boris", "Carla"]
--
-- If your 'In'-wrapped list is empty, the string @\"(null)\"@ will be
-- substituted instead, to ensure that your clause remains
-- syntactically valid.

-- $many
--
-- If you know that you have many rows of data to insert into a table,
-- it is much more efficient to perform all the insertions in a single
-- multi-row @INSERT@ statement than individually.
--
-- The 'executeMany' function is intended specifically for helping
-- with multi-row @INSERT@ and @UPDATE@ statements. Its rules for
-- query substitution are different than those for 'execute'.
--
-- What 'executeMany' searches for in your 'Query' template is a
-- single substring of the form:
--
-- > values (?,?,?)
--
-- The rules are as follows:
--
-- * The keyword @VALUES@ is matched case insensitively.
--
-- * There must be no other \"@?@\" characters anywhere in your
--   template.
--
-- * There must be one or more \"@?@\" in the parentheses.
--
-- * Extra white space is fine.
--
-- The last argument to 'executeMany' is a list of parameter
-- tuples. These will be substituted into the query where the @(?,?)@
-- string appears, in a form suitable for use in a multi-row @INSERT@
-- or @UPDATE@.
--
-- Here is an example:
--
-- > executeMany conn
-- >   "insert into users (first_name,last_name) values (?,?)"
-- >   [("Boris","Karloff"),("Ed","Wood")]
--
-- The query that will be executed here will look like this
-- (reformatted for tidiness):
--
-- > insert into users (first_name,last_name) values
-- >   ('Boris','Karloff'),('Ed','Wood')

-- $returning
--
-- PostgreSQL supports returning values from data manipulation statements
-- such as @INSERT@ and @UPDATE@.   You can use these statements by
-- using 'query' instead of 'execute'.   For multi-tuple inserts,
-- use 'returning' instead of 'executeMany'.
--
-- For example, were there an auto-incrementing @id@ column and
-- timestamp column @t@ that defaulted to the present time for the
-- @sales@ table, then the following query would insert two new
-- sales records and also return their new @id@s and timestamps.
--
-- > let q = "insert into sales (amount, label) values (?,?) returning id, t"
-- > xs :: [(Int, UTCTime)] <- query conn q (15,"Sawdust")
-- > ys :: [(Int, UTCTime)] <- returning conn q [(20,"Chips"),(300,"Wood")]

-- $result
--
-- The 'query' and 'query_' functions return a list of values in the
-- 'FromRow' typeclass. This class performs automatic extraction
-- and type conversion of rows from a query result.
--
-- Here is a simple example of how to extract results:
--
-- > import qualified Data.Text as Text
-- >
-- > xs <- query_ conn "select name,age from users"
-- > forM_ xs $ \(name,age) ->
-- >   putStrLn $ Text.unpack name ++ " is " ++ show (age :: Int)
--
-- Notice two important details about this code:
--
-- * The number of columns we ask for in the query template must
--   exactly match the number of elements we specify in a row of the
--   result tuple.  If they do not match, a 'ResultError' exception
--   will be thrown.
--
-- * Sometimes, the compiler needs our help in specifying types. It
--   can infer that @name@ must be a 'Text', due to our use of the
--   @unpack@ function. However, we have to tell it the type of @age@,
--   as it has no other information to determine the exact type.

-- $null
--
-- The type of a result tuple will look something like this:
--
-- > (Text, Int, Int)
--
-- Although SQL can accommodate @NULL@ as a value for any of these
-- types, Haskell cannot. If your result contains columns that may be
-- @NULL@, be sure that you use 'Maybe' in those positions of your
-- tuple.
--
-- > (Text, Maybe Int, Int)
--
-- If 'query' encounters a @NULL@ in a row where the corresponding
-- Haskell type is not 'Maybe', it will throw a 'ResultError'
-- exception.

-- $only_result
--
-- To specify that a query returns a single-column result, use the
-- 'Only' type.
--
-- > xs <- query_ conn "select id from users"
-- > forM_ xs $ \(Only dbid) -> {- ... -}

-- $types
--
-- Conversion of SQL values to Haskell values is somewhat
-- permissive. Here are the rules.
--
-- * For numeric types, any Haskell type that can accurately represent
--   all values of the given PostgreSQL type is considered \"compatible\".
--   For instance, you can always extract a PostgreSQL 16-bit @SMALLINT@
--   column to a Haskell 'Int'.  The Haskell 'Float' type can accurately
--   represent a @SMALLINT@, so it is considered compatble with those types.
--
-- * A numeric compatibility check is based only on the type of a
--   column, /not/ on its values. For instance, a PostgreSQL 64-bit
--   @BIGINT@ column will be considered incompatible with a Haskell
--   'Int16', even if it contains the value @1@.
--
-- * If a numeric incompatibility is found, 'query' will throw a
--   'ResultError'.
--
-- * The 'String' and 'Text' types are assumed to be encoded as
--   UTF-8. If you use some other encoding, decoding may fail or give
--   wrong results. In such cases, write a @newtype@ wrapper and a
--   custom 'Result' instance to handle your encoding.
