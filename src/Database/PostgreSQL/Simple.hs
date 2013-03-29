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
-- Portability: portable
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

    -- ** @RETURNING@: modifications that returns results
    -- $returning

    -- * Extracting results
    -- $result

    -- ** Handling null values
    -- $null

    -- ** Type conversions
    -- $types

    -- * Types
      Base.ConnectInfo(..)
    , Connection
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
    , FormatError(fmtMessage, fmtQuery, fmtParams)
    , QueryError(qeMessage, qeQuery)
    , ResultError(errSQLType, errHaskellType, errMessage)
    -- * Connection management
    , Base.connect
    , Base.connectPostgreSQL
    , Base.postgreSQLConnectionString
    , Base.defaultConnectInfo
    , Base.close
    -- * Queries that return results
    , query
    , query_
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
    -- * Statements that do not return results
    , execute
    , execute_
    , executeMany
--    , Base.insertID
    -- * Transaction handling
    , withTransaction
--    , Base.autocommit
    , begin
    , commit
    , rollback
    -- * Helper functions
    , formatMany
    , formatQuery
    ) where

import           Blaze.ByteString.Builder
                   ( Builder, fromByteString, toByteString )
import           Blaze.ByteString.Builder.Char8 (fromChar)
import           Control.Applicative ((<$>), pure)
import           Control.Exception
                   ( Exception, throw, throwIO, finally )
import           Control.Monad (foldM)
import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import           Data.List (intersperse)
import           Data.Monoid (mconcat)
import           Data.Typeable (Typeable)
import           Database.PostgreSQL.Simple.Compat ( (<>) )
import           Database.PostgreSQL.Simple.FromField (ResultError(..))
import           Database.PostgreSQL.Simple.FromRow (FromRow(..))
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.ToField (Action(..), inQuotes)
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.Types
                   ( Binary(..), In(..), Only(..), Query(..), (:.)(..) )
import           Database.PostgreSQL.Simple.Internal as Base
import           Database.PostgreSQL.Simple.Transaction
import           Database.PostgreSQL.Simple.TypeInfo
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict

-- | Exception thrown if a 'Query' could not be formatted correctly.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError {
      fmtMessage :: String
    , fmtQuery :: Query
    , fmtParams :: [ByteString]
    } deriving (Eq, Show, Typeable)

instance Exception FormatError

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
      return . toByteString . mconcat $ fromByteString before :
                                        intersperse (fromChar ',') bs ++
                                        [fromByteString after]
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

escapeStringConn :: Connection -> ByteString -> IO (Either ByteString ByteString)
escapeStringConn conn s =
    withConnection conn $ \c ->
    PQ.escapeStringConn c s >>= checkError c

escapeByteaConn :: Connection -> ByteString -> IO (Either ByteString ByteString)
escapeByteaConn conn s =
    withConnection conn $ \c ->
    PQ.escapeByteaConn c s >>= checkError c

checkError :: PQ.Connection -> Maybe a -> IO (Either ByteString a)
checkError _ (Just x) = return $ Right x
checkError c Nothing  = Left . maybe "" id <$> PQ.errorMessage c

buildQuery :: Connection -> Query -> ByteString -> [Action] -> IO Builder
buildQuery conn q template xs = zipParams (split template) <$> mapM sub xs
  where quote = either (\msg -> fmtError (utf8ToString msg) q xs)
                       (inQuotes . fromByteString)
        utf8ToString = T.unpack . TE.decodeUtf8
        sub (Plain  b)      = pure b
        sub (Escape s)      = quote <$> escapeStringConn conn s
        sub (EscapeByteA s) = quote <$> escapeByteaConn conn s
        sub (Many  ys)      = mconcat <$> mapM sub ys
        split s = fromByteString h : if B.null t then [] else split (B.tail t)
            where (h,t) = B.break (=='?') s
        zipParams (t:ts) (p:ps) = t <> p <> zipParams ts ps
        zipParams [t] []        = t
        zipParams _ _ = fmtError (show (B.count '?' template) ++
                                  " '?' characters, but " ++
                                  show (length xs) ++ " parameters") q xs

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
execute :: (ToRow q) => Connection -> Query -> q -> IO Int64
execute conn template qs = do
  result <- exec conn =<< formatQuery conn template qs
  finishExecute conn template result

-- | Execute a multi-row @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
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
-- Throws 'FormatError' if the query could not be formatted correctly.
returning :: (ToRow q, FromRow r) => Connection -> Query -> [q] -> IO [r]
returning _ _ [] = return []
returning conn q qs = do
  result <- exec conn =<< formatMany conn q qs
  finishQuery conn q result

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
query :: (ToRow q, FromRow r)
         => Connection -> Query -> q -> IO [r]
query conn template qs = do
  result <- exec conn =<< formatQuery conn template qs
  finishQuery conn template result

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ conn q@(Query que) = do
  result <- exec conn que
  finishQuery conn q result

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
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.

fold            :: ( FromRow row, ToRow params )
                => Connection
                -> Query
                -> params
                -> a
                -> (a -> row -> IO a)
                -> IO a
fold = foldWithOptions defaultFoldOptions

data FetchQuantity
   = Automatic
   | Fixed !Int

data FoldOptions
   = FoldOptions {
       fetchQuantity   :: !FetchQuantity,
       transactionMode :: !TransactionMode
     }

defaultFoldOptions :: FoldOptions
defaultFoldOptions = FoldOptions {
      fetchQuantity   = Automatic,
      transactionMode = TransactionMode ReadCommitted ReadOnly
    }

foldWithOptions :: ( FromRow row, ToRow params )
                => FoldOptions
                -> Connection
                -> Query
                -> params
                -> a
                -> (a -> row -> IO a)
                -> IO a
foldWithOptions opts conn template qs a f = do
    q <- formatQuery conn template qs
    doFold opts conn template (Query q) a f

-- | A version of 'fold' that does not perform query substitution.
fold_ :: (FromRow r) =>
         Connection
      -> Query                  -- ^ Query.
      -> a                      -- ^ Initial state for result consumer.
      -> (a -> r -> IO a)       -- ^ Result consumer.
      -> IO a
fold_ = foldWithOptions_ defaultFoldOptions


foldWithOptions_ :: (FromRow r) =>
                    FoldOptions
                 -> Connection
                 -> Query             -- ^ Query.
                 -> a                 -- ^ Initial state for result consumer.
                 -> (a -> r -> IO a)  -- ^ Result consumer.
                 -> IO a
foldWithOptions_ opts conn query a f = doFold opts conn query query a f


doFold :: ( FromRow row )
       => FoldOptions
       -> Connection
       -> Query
       -> Query
       -> a
       -> (a -> row -> IO a)
       -> IO a
doFold FoldOptions{..} conn _template q a f = do
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
    go = do
       -- FIXME:  what about name clashes with already-declared cursors?
       _ <- execute_ conn ("DECLARE fold NO SCROLL CURSOR FOR " <> q)
       loop a `finally` execute_ conn "CLOSE fold"

-- FIXME: choose the Automatic chunkSize more intelligently
--   One possibility is to use the type of the results,  although this
--   still isn't a perfect solution, given that common types (e.g. text)
--   are of highly variable size.
--   A refinement of this technique is to pick this number adaptively
--   as results are read in from the database.
    chunkSize = case fetchQuantity of
                 Automatic   -> 256
                 Fixed n     -> n
    loop a = do
      rs <- query conn "FETCH FORWARD ? FROM fold" (Only chunkSize)
      if null rs then return a else foldM f a rs >>= loop

-- | A version of 'fold' that does not transform a state value.
forEach :: (ToRow q, FromRow r) =>
           Connection
        -> Query                -- ^ Query template.
        -> q                    -- ^ Query parameters.
        -> (r -> IO ())         -- ^ Result consumer.
        -> IO ()
forEach conn template qs = fold conn template qs () . const
{-# INLINE forEach #-}

-- | A version of 'forEach' that does not perform query substitution.
forEach_ :: (FromRow r) =>
            Connection
         -> Query                -- ^ Query template.
         -> (r -> IO ())         -- ^ Result consumer.
         -> IO ()
forEach_ conn template = fold_ conn template () . const
{-# INLINE forEach_ #-}

forM' :: (Ord n, Num n) => n -> n -> (n -> IO a) -> IO [a]
forM' lo hi m = loop hi []
  where
    loop !n !as
      | n < lo = return as
      | otherwise = do
           a <- m n
           loop (n-1) (a:as)

finishQuery :: (FromRow r) => Connection -> Query -> PQ.Result -> IO [r]
finishQuery conn q result = do
  status <- PQ.resultStatus result
  case status of
    PQ.EmptyQuery ->
        throwIO $ QueryError "query: Empty query" q
    PQ.CommandOk -> do
        throwIO $ QueryError "query resulted in a command response" q
    PQ.TuplesOk -> do
        let unCol (PQ.Col x) = fromIntegral x :: Int
        nrows <- PQ.ntuples result
        ncols <- PQ.nfields result
        forM' 0 (nrows-1) $ \row -> do
           let rw = Row row result
           okvc <- runConversion (runStateT (runReaderT (unRP fromRow) rw) 0) conn
           case okvc of
             Ok (val,col) | col == ncols -> return val
                          | otherwise -> do
                              vals <- forM' 0 (ncols-1) $ \c -> do
                                  tinfo <- getTypeInfo conn =<< PQ.ftype result c
                                  v <- PQ.getvalue result row c
                                  return ( tinfo
                                         , fmap ellipsis v       )
                              throw (ConversionFailed
                               (show (unCol ncols) ++ " values: " ++ show vals)
                               Nothing
                               ""
                               (show (unCol col) ++ " slots in target type")
                               "mismatch between number of columns to \
                               \convert and number in target type")
             Errors []  -> throwIO $ ConversionFailed "" Nothing "" "" "unknown error"
             Errors [x] -> throwIO x
             Errors xs  -> throwIO $ ManyErrors xs
    PQ.CopyOut ->
        throwIO $ QueryError "query: COPY TO is not supported" q
    PQ.CopyIn ->
        throwIO $ QueryError "query: COPY FROM is not supported" q
    PQ.BadResponse   -> throwResultError "query" result status
    PQ.NonfatalError -> throwResultError "query" result status
    PQ.FatalError    -> throwResultError "query" result status

ellipsis :: ByteString -> ByteString
ellipsis bs
    | B.length bs > 15 = B.take 10 bs `B.append` "[...]"
    | otherwise        = bs

fmtError :: String -> Query -> [Action] -> a
fmtError msg q xs = throw FormatError {
                      fmtMessage = msg
                    , fmtQuery = q
                    , fmtParams = map twiddle xs
                    }
  where twiddle (Plain b)       = toByteString b
        twiddle (Escape s)      = s
        twiddle (EscapeByteA s) = s
        twiddle (Many ys)       = B.concat (map twiddle ys)

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
-- > hello = do
-- >   conn <- connect defaultConnectInfo
-- >   query conn "select 2 + 2"
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

-- $only_param
--
-- Haskell lacks a single-element tuple type, so if you have just one
-- value you want substituted into a query, what should you do?
--
-- The obvious approach would appear to be something like this:
--
-- > instance (Param a) => QueryParam a where
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
-- >       In ["Anna", "Boris", "Carla"]
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
-- The 'returning' function is similar to 'executeMany' but is
-- intended for use with multi-tuple @INSERT@ or @UPDATE@ statements
-- that make use of @RETURNING@.
--
-- For example, were there an auto-incrementing @id@ column and
-- timestamp column @t@ that defaulted to the present time for the
-- @sales@ table, then the following query would insert two new
-- sales records and also return their new @id@s and timestamps.
--
-- > let q = "insert into sales (amount, label) values (?,?) returning id, t"
-- > xs :: [(Int, UTCTime)] <- returning conn q [(20,"Chips"),(300,"Wood")]

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
