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
-- @
-- runghc -itools tools/GenTypeInfo.hs
-- @
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
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Builder            as Blaze
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
    , rngsubtype  :: Maybe Oid
    }

instance FromRow TypeInfo where 
    fromRow = TypeInfo <$> field <*> field <*> field <*> field <*> field <*> field

type NameMap   = Map.Map B8.ByteString TypeInfo

type OidMap    = Map.Map Oid TypeInfo

type TypeName = (B8.ByteString, B8.ByteString)

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
_record                  array_record
regprocedure
regoper
regoperator
regclass
regtype
uuid
json
jsonb
int2vector
oidvector
_xml                     array_xml
_json                    array_json
_line                    array_line
_cidr                    array_cidr
_circle                  array_circle
_money                   array_money
_bool                    array_bool
_bytea                   array_bytea
_char                    array_char
_name                    array_name
_int2                    array_int2
_int2vector              array_int2vector
_int4                    array_int4
_regproc                 array_regproc
_text                    array_text
_tid                     array_tid
_xid                     array_xid
_cid                     array_cid
_oidvector               array_oidvector
_bpchar                  array_bpchar
_varchar                 array_varchar
_int8                    array_int8
_point                   array_point
_lseg                    array_lseg
_path                    array_path
_box                     array_box
_float4                  array_float4
_float8                  array_float8
_polygon                 array_polygon
_oid                     array_oid
_macaddr                 array_macaddr
_inet                    array_inet
_timestamp               array_timestamp
_date                    array_date
_time                    array_time
_timestamptz             array_timestamptz
_interval                array_interval
_numeric                 array_numeric
_timetz                  array_timetz
_bit                     array_bit
_varbit                  array_varbit
_refcursor               array_refcursor
_regprocedure            array_regprocedure
_regoper                 array_regoper
_regoperator             array_regoperator
_regclass                array_regclass
_regtype                 array_regtype
_uuid                    array_uuid
_jsonb                   array_jsonb
int4range
_int4range
numrange
_numrange
tsrange
_tsrange
tstzrange
_tstzrange
daterange
_daterange
int8range
_int8range
|]

connectionString = "dbname=postgres"

withPostgreSQL = bracket (connectPostgreSQL connectionString) close

getTypeInfos :: TypeNames -> IO (OidMap, NameMap)
getTypeInfos typnames = withPostgreSQL $ \conn -> do
    infos <- query conn [sql| 
         WITH types AS 
            (SELECT oid, typcategory, typdelim, typname, typelem
               FROM pg_type WHERE typname IN ?)
            SELECT types.*, rngsubtype FROM types LEFT JOIN pg_range ON oid = rngtypid
      |] (Only (In (sort (map pg typnames))))
    loop conn (oidMap infos) (nameMap infos) infos
  where
    oidMap  = Map.fromList . map (typoid  &&& id)
    nameMap = Map.fromList . map (typname &&& id)
    loop conn oids names infos = do
      let unknowns = [ x | x <- map typelem infos ++
                                  [ x | Just x <- map rngsubtype infos ],
                           x /= Oid 0,
                           not (Map.member x oids) ]
      case unknowns of
        []    -> return (oids, names)
        (_:_) -> do
           infos' <- query conn [sql|
             WITH types AS 
               (SELECT oid, typcategory, typdelim, typname, typelem
                  FROM pg_type WHERE oid IN ?)
               SELECT types.*, rngsubtype 
                 FROM types LEFT JOIN pg_range ON oid = rngtypid
             |] (Only (In (sort unknowns)))
           let oids'  = oids  `Map.union` oidMap  infos'
               names' = names `Map.union` nameMap infos'
           loop conn oids' names' infos'

main = do
  (oidmap, namemap) <- getTypeInfos typeNames
  L.writeFile "src/Database/PostgreSQL/Simple/TypeInfo/Static.hs"
              (Blaze.toLazyByteString (renderFile oidmap namemap typeNames))


showOid (Oid n) = show n

renderOid :: NameMap -> TypeName -> Blaze.Builder
renderOid byName name
  = case Map.lookup (pg name) byName of
      Nothing -> error (B8.unpack (pg name))
      Just (showOid . typoid -> n) -> fromString n
                              ++ fromString (replicate (4 - length n) ' ')

renderElem :: OidMap -> Oid -> Blaze.Builder
renderElem byOid elemOid
  | elemOid == Oid 0 = "Nothing"
  | otherwise = case Map.lookup elemOid byOid of
                  Nothing -> error ("oid not found: " ++ show elemOid)
                  Just x  -> "Just " ++ bs (typname x)

renderTypeInfo :: OidMap -> TypeInfo -> TypeName -> Blaze.Builder
renderTypeInfo byOid info name
  | typcategory info == 'A' || typname info == "_record" =
     let (Just typelem_info)    = Map.lookup (typelem info) byOid
         typelem_hs_name =
             case lookup (typname typelem_info) typeNames of
               Nothing -> error (   "type not found: "
                                 ++ B8.unpack( typname typelem_info)
                                 ++ " (typelem of " ++ B8.unpack (typname info)
                                 ++ ")")
               Just x  -> x
      in concat
           [ "\n"
           , bs (hs name), " :: TypeInfo\n"
           , bs (hs name), " =  Array {\n"
           , "    typoid      = ", bs (hs name), "Oid,\n"
           , "    typcategory = '", bs $ B8.singleton (typcategory info), "',\n"
           , "    typdelim    = '", bs $ B8.singleton (typdelim info), "',\n"
           , "    typname     = \"", bs (typname info), "\",\n"
           , "    typelem     = ", bs typelem_hs_name, "\n"
           , "  }\n"
           , "\n"
           , bs (hs name), "Oid :: Oid\n"
           , bs (hs name), "Oid = ", fromString (show (typoid info)), "\n"
           , "{-# INLINE ", bs (hs name), "Oid #-}"
           , "\n"
           ]
  | typcategory info == 'R' =
      let (Just rngsubtype_oid)  = rngsubtype info
          (Just rngsubtype_info) = Map.lookup rngsubtype_oid byOid
          rngsubtype_hs_name =
              case lookup (typname rngsubtype_info) typeNames of
                Nothing -> error (   "type not found: "
                                  ++ B8.unpack (typname rngsubtype_info)
                                  ++ " (rngsubtype of "
                                  ++ B8.unpack (typname info) ++ ")")
                Just x  -> x
       in concat
           [ "\n"
           , bs (hs name), " :: TypeInfo\n"
           , bs (hs name), " =  Range {\n"
           , "    typoid      = ", bs (hs name), "Oid,\n"
           , "    typcategory = '", bs $ B8.singleton (typcategory info), "',\n"
           , "    typdelim    = '", bs $ B8.singleton (typdelim info), "',\n"
           , "    typname     = \"", bs (typname info), "\",\n"
           , "    rngsubtype  = ", bs rngsubtype_hs_name, "\n"
           , "  }\n"
           , "\n"
           , bs (hs name), "Oid :: Oid\n"
           , bs (hs name), "Oid = ", fromString (show (typoid info)), "\n"
           , "{-# INLINE ", bs (hs name), "Oid #-}"
           , "\n"
           ]
  | otherwise =
         concat
           [ "\n"
           , bs (hs name), " :: TypeInfo\n"
           , bs (hs name), " =  Basic {\n"
           , "    typoid      = ", bs (hs name), "Oid,\n"
           , "    typcategory = '", bs $ B8.singleton (typcategory info), "',\n"
           , "    typdelim    = '", bs $ B8.singleton (typdelim info), "',\n"
           , "    typname     = \"", bs (typname info), "\"\n"
           , "  }\n"
           , "\n"
           , bs (hs name), "Oid :: Oid\n"
           , bs (hs name), "Oid = ", fromString (show (typoid info)), "\n"
           , "{-# INLINE ", bs (hs name), "Oid #-}"
           , "\n"
           ]

-- FIXME:  add in any names that we need that we didn't specify, (i.e.
--         the "unknowns" in getTypeInfos
--         and munge them into a valid haskell identifier if needed.
getNames :: NameMap -> TypeNames -> TypeNames
getNames _ x = x

bs = Blaze.byteString

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
            ++ "     , " ++ bs (hs name) ++ "Oid\n"
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
   ++ concat [ renderTypeInfo byOid typeInfo name
             | name <- getNames byName names
             ,  let (Just typeInfo) = Map.lookup (pg name) byName])
