{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Types
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Basic types.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Types
    (
      Null(..)
    , Default(..)
    , Only(..)
    , In(..)
    , Binary(..)
    , Identifier(..)
    , QualifiedIdentifier(..)
    , Query(..)
    , Oid(..)
    , (:.)(..)
    , Savepoint(..)
    , PGArray(..)
    , Values(..)
    ) where

import           Blaze.ByteString.Builder (toByteString)
import           Control.Arrow (first)
import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Monoid (Monoid(..))
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.PostgreSQL.LibPQ (Oid(..))

-- | A placeholder for the SQL @NULL@ value.
data Null = Null
          deriving (Read, Show, Typeable)

instance Eq Null where
    _ == _ = False
    _ /= _ = False

-- | A placeholder for the PostgreSQL @DEFAULT@ value.
data Default = Default
           deriving (Read, Show, Typeable)

-- | A query string. This type is intended to make it difficult to
-- construct a SQL query by concatenating string fragments, as that is
-- an extremely common way to accidentally introduce SQL injection
-- vulnerabilities into an application.
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.PostgreSQL.Simple
-- >
-- > q :: Query
-- > q = "select ?"
--
-- The underlying type is a 'ByteString', and literal Haskell strings
-- that contain Unicode characters will be correctly transformed to
-- UTF-8.
newtype Query = Query {
      fromQuery :: ByteString
    } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . toByteString . Utf8.fromString

instance Monoid Query where
    mempty = Query B.empty
    mappend (Query a) (Query b) = Query (B.append a b)
    {-# INLINE mappend #-}
    mconcat xs = Query (B.concat (map fromQuery xs))

-- | A single-value \"collection\".
--
-- This is useful if you need to supply a single parameter to a SQL
-- query, or extract a single column from a SQL result.
--
-- Parameter example:
--
-- @query c \"select x from scores where x > ?\" ('Only' (42::Int))@
--
-- Result example:
--
-- @xs <- query_ c \"select id from users\"
--forM_ xs $ \\('Only' id) -> {- ... -}@
newtype Only a = Only {
      fromOnly :: a
    } deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap a list of values for use in an @IN@ clause.  Replaces a
-- single \"@?@\" character with a parenthesized list of rendered
-- values.
--
-- Example:
--
-- > query c "select * from whatever where id in ?" (Only (In [3,4,5]))
newtype In a = In a
    deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap binary data for use as a @bytea@ value.
newtype Binary a = Binary {fromBinary :: a}
    deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | Wrap text for use as sql identifier, i.e. a table or column name.
newtype Identifier = Identifier {fromIdentifier :: Text}
    deriving (Eq, Ord, Read, Show, Typeable, IsString)

instance Hashable Identifier where
    hashWithSalt i (Identifier t) = hashWithSalt i t

-- | Wrap text for use as (maybe) qualified identifier, i.e. a table
-- with schema, or column with table.
data QualifiedIdentifier = QualifiedIdentifier (Maybe Text) Text
    deriving (Eq, Ord, Read, Show, Typeable)

instance Hashable QualifiedIdentifier where
    hashWithSalt i (QualifiedIdentifier q t) = hashWithSalt i (q, t)

instance IsString QualifiedIdentifier where
    fromString str = let (x,y) = T.break (== '.') (fromString str)
                      in if T.null y
                         then QualifiedIdentifier Nothing x
                         else QualifiedIdentifier (Just x) (T.tail y)

-- | Wrap a list for use as a PostgreSQL array.
newtype PGArray a = PGArray {fromPGArray :: [a]}
    deriving (Eq, Ord, Read, Show, Typeable, Functor)

-- | A composite type to parse your custom data structures without
-- having to define dummy newtype wrappers every time.
--
--
-- > instance FromRow MyData where ...
--
-- > instance FromRow MyData2 where ...
--
--
-- then I can do the following for free:
--
-- @
-- res <- query' c "..."
-- forM res $ \\(MyData{..} :. MyData2{..}) -> do
--   ....
-- @
data h :. t = h :. t deriving (Eq,Ord,Show,Read,Typeable)

infixr 3 :.

newtype Savepoint = Savepoint Query
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Represents a @VALUES@ table literal,  usable as an alternative
--   to @executeMany@ and @returning@.  For example:
--
-- > execute c "INSERT INTO table (key,val) ?"
-- >      (Only (Values ["int4","text"]
-- >                    [(1,"hello"),(2,"world")]))
--
--   Issues the following query:
--
-- > INSERT INTO table (key,val) (VALUES (1::"int4",'hello'::"text"),(2,'world'))
--
--   When the list of values is empty,  the following query will be issued:
--
-- > INSERT INTO table (key,val) (VALUES (null::"int4",null::"text") LIMIT 0)
--
--   By contrast, @executeMany@ and @returning@ don't issue the query
--   in the empty case, and simply return @0@ and @[]@ respectively.
--
--   The advantage over @executeMany@ is in cases when you want to
--   parameterize table literals in addition to other parameters,  as can
--   occur with writable common table expressions, for example.
--
--   The first argument is a list of postgresql type names.  Because this
--   is turned into a properly quoted identifier,  the type name is case
--   sensitive and must be as it appears in the @pg_type@ table.   Thus,
--   you must write @timestamptz@ instead of @timestamp with time zone@,
--   @int4@ instead of @integer@, @_int8@ instead of @bigint[]@, etcetera.
--
--   You may omit the type names,  however,  if you do so the list
--   of values must be non-empty,  and postgresql must be able to infer
--   the types of the columns from the surrounding context.   If the first
--   condition is not met,  postgresql-simple will throw an exception
--   without issuing the query.   In the second case,  the postgres server
--   will return an error which will be turned into a @SqlError@ exception.
--
--   See <http://www.postgresql.org/docs/9.3/static/sql-values.html> for
--   more information.
data Values a = Values [QualifiedIdentifier] [a]
    deriving (Eq, Ord, Show, Read, Typeable)
