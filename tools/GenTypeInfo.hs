------------------------------------------------------------------------------
-- |
-- Module:      GenTypeInfo
-- Copyright:   (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Queries a PostgreSQL database for the Object IDs and other type
-- information associated with typenames,  and generates a module with
-- data constants representing part of the pg_type table.
--
-- Note that only some of the built-in types have stable type OIDs,  and
-- thus a TypeInfo module that contains user-defined types is liable to
-- not work across different database instances.   For these types, it is
-- better for `FromField` to use the `typeinfo` operator that works for
-- any type,  whether or not it is in this module.
--
-- `typeinfo` works because postgresql-simple will dynamically query
-- the pg_types table the first time it receives a type OID it doesn't
-- know about.   It then constructs a TypeInfo record and stores it in
-- a per-connection cache for later use.
--
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, RecordWildCards #-}

module GenBuiltinTypes where

import Prelude hiding ((++), concat)

import           StringsQQ
import           Control.Arrow((&&&))
import           Control.Applicative
import           Control.Exception(bracket)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.Types(Oid(..))
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString.Char8 as B
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy  as L
import qualified Blaze.ByteString.Builder            as Blaze
import qualified Blaze.ByteString.Builder.ByteString as Blaze
import qualified Blaze.ByteString.Builder.Char8      as Blaze
import           Data.String
import           Data.List ( sort, intersperse )
import qualified Data.Map as Map

import Data.Monoid

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

concat :: Monoid a => [a] -> a
concat = mconcat

data TypeInfo = TypeInfo
    { typoid      :: Oid
    , typcategory :: Char
    , typdelim    :: Char
    , typname     :: ByteString
    , typelem     :: Oid
    }

instance FromRow TypeInfo where
    fromRow = TypeInfo <$> field <*> field <*> field <*> field <*> field

type NameMap   = Map.Map B.ByteString TypeInfo

type OidMap    = Map.Map Oid TypeInfo

type TypeName = (B.ByteString, B.ByteString)

type TypeNames = [TypeName]

-- Note that the following syntax is "pgName <whitespace> hsName",  though
-- they default to the same thing if there is only one identifier
typeNames :: TypeNames
typeNames = [typenames|
bool
bytea
char
name
int8
int2
int4
regproc
text
oid
tid
xid
cid
xml
point
lseg
path
box
polygon
line
cidr
float4
float8
abstime
reltime
tinterval
unknown
circle
money
macaddr
inet
bpchar
varchar
date
time
timestamp
timestamptz
interval
timetz
bit
varbit
numeric
refcursor
record
void
uuid
json
jsonb
|]

instance IsString Blaze.Builder where
   fromString = Blaze.fromByteString . fromString

connectionString = "dbname=postgres"

withPostgreSQL = bracket (connectPostgreSQL connectionString) close

getTypeInfos :: TypeNames -> IO (OidMap, NameMap)
getTypeInfos typnames = withPostgreSQL $ \conn -> do
    infos <- query conn [sql|
                SELECT oid, typcategory, typdelim, typname, typelem
                FROM   pg_type
                WHERE  typname IN ?
              |]
              (Only (In (sort (map pg typnames))))
    loop conn (oidMap infos) (nameMap infos) infos
  where
    oidMap  = Map.fromList . map (typoid  &&& id)
    nameMap = Map.fromList . map (typname &&& id)
    loop conn oids names infos = do
      let unknowns = [ x | x <- map typelem infos,
                           x /= Oid 0,
                           not (Map.member x oids) ]
      case unknowns of
        []    -> return (oids, names)
        (_:_) -> do
           infos' <- query conn [sql|
                         SELECT oid, typcategory, typdelim, typname, typelem
                         FROM   pg_type
                         WHERE  oid IN ?
                       |] (Only (In (sort unknowns)))
           let oids'  = oids  `Map.union` oidMap  infos'
               names' = names `Map.union` nameMap infos'
           loop conn oids' names' infos'

main = do
  (oidmap, namemap) <- getTypeInfos typeNames
  L.writeFile "../src/Database/PostgreSQL/Simple/TypeInfo/Static.hs"
              (Blaze.toLazyByteString (renderFile oidmap namemap typeNames))


showOid (Oid n) = show n

renderOid :: NameMap -> TypeName -> Blaze.Builder
renderOid byName name
  = case Map.lookup (pg name) byName of
      Nothing -> error (B.unpack (pg name))
      Just (showOid . typoid -> n) -> fromString n
                              ++ fromString (replicate (4 - length n) ' ')

renderElem :: OidMap -> Oid -> Blaze.Builder
renderElem byOid elemOid
  | elemOid == Oid 0 = "Nothing"
  | otherwise = case Map.lookup elemOid byOid of
                  Nothing -> error ("oid not found: " ++ show elemOid)
                  Just x  -> "Just " ++ bs (typname x)

-- FIXME:  add in any names that we need that we didn't specify, (i.e.
--         the "unknowns" in getTypeInfos
--         and munge them into a valid haskell identifier if needed.
getNames :: NameMap -> TypeNames -> TypeNames
getNames _ x = x

bs = Blaze.fromByteString

pg = fst

hs = snd

renderFile :: OidMap -> NameMap -> TypeNames -> Blaze.Builder
renderFile byOid byName names = ([longstring|
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.TypeInfo
-- Copyright:   (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- This module contains portions of the @pg_type@ table that are relevant
-- to postgresql-simple and are believed to not change between PostgreSQL
-- versions.
--
------------------------------------------------------------------------------

-- Note that this file is generated by tools/GenTypeInfo.hs, and should
-- not be edited directly

module Database.PostgreSQL.Simple.TypeInfo.Static
     ( TypeInfo(..)
     , staticTypeInfo
|] ++ concat [ "     , " ++ bs (hs name) ++ "\n"
             | name <- names ] ++ [longstring|
     ) where

import Database.PostgreSQL.LibPQ (Oid(..))
import Database.PostgreSQL.Simple.TypeInfo.Types

staticTypeInfo :: Oid -> Maybe TypeInfo
staticTypeInfo (Oid x) = case x of
|] ++ concat [concat [ "    ", renderOid byName name,
                                  " -> Just ", bs (hs name), "\n"
                     ]
             | name <- names ]
   ++ [longstring|
    _ -> Nothing
|]
   ++ concat [concat
              [ "\n"
              , bs (hs name), " :: TypeInfo\n"
              , bs (hs name), " =  Basic {\n"
              , "    typoid      = ", fromString (show typoid), ",\n"
              , "    typcategory = '", Blaze.fromChar typcategory, "',\n"
              , "    typdelim    = '", Blaze.fromChar typdelim, "',\n"
              , "    typname     = \"", bs typname, "\"\n"
              , "  }\n"
              ]
             | name <- getNames byName names
             ,  let (Just (TypeInfo{..})) = Map.lookup (pg name) byName])
