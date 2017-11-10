{-# LANGUAGE CPP, ViewPatterns, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.HStore.Implementation
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- This code has yet to be profiled and optimized.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.HStore.Implementation where

import GHC.Stack
import           Control.Applicative
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P (isSpace_w8)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder, byteString, char8)
import qualified Data.ByteString.Builder as BU
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy          as BL
#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Internal as BL (foldrChunks)
#endif
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Text(Text)
import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as TS
import           Data.Text.Encoding.Error(UnicodeException)
import qualified Data.Text.Lazy          as TL
import           Data.Typeable
import           Data.Monoid(Monoid(..))
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import qualified Data.List as DL

class ToHStore a where
   toHStore :: a -> HStoreBuilder

-- | Represents valid hstore syntax.
data HStoreBuilder
   = Empty
   | Comma !Builder
     deriving (Typeable)

instance ToHStore HStoreBuilder where
   toHStore = id

toBuilder :: HStoreBuilder -> Builder
toBuilder x = case x of
                Empty -> mempty
                Comma x -> x

toLazyByteString :: HStoreBuilder -> BL.ByteString
toLazyByteString x = case x of
                       Empty -> BL.empty
                       Comma x -> BU.toLazyByteString x

instance Monoid HStoreBuilder where
    mempty = Empty
    mappend Empty     x = x
    mappend (Comma a) x
        = Comma (a `mappend` case x of
                               Empty   -> mempty
                               Comma b -> char8 ',' `mappend` b)

class ToHStoreText a where
  toHStoreText :: a -> HStoreText

-- | Represents escape text, ready to be the key or value to a hstore value
newtype HStoreText = HStoreText Builder deriving (Typeable, Monoid)

instance ToHStoreText HStoreText where
  toHStoreText = id

-- | Assumed to be UTF-8 encoded
instance ToHStoreText BS.ByteString where
  toHStoreText str = HStoreText (escapeAppend str mempty)

-- | Assumed to be UTF-8 encoded
instance ToHStoreText BL.ByteString where
  toHStoreText = HStoreText . BL.foldrChunks escapeAppend mempty

instance ToHStoreText TS.Text where
  toHStoreText str = HStoreText (escapeAppend (TS.encodeUtf8 str) mempty)

instance ToHStoreText TL.Text where
  toHStoreText = HStoreText . TL.foldrChunks (escapeAppend . TS.encodeUtf8) mempty

escapeAppend :: (HasCallStack) => BS.ByteString -> Builder -> Builder
escapeAppend = loop
  where
    loop (BS.break quoteNeeded -> (a,b)) rest
      = byteString a `mappend`
          case BS.uncons b of
            Nothing     ->  rest
            Just (c,d)  ->  quoteChar c `mappend` loop d rest

    quoteNeeded c = c == c2w '\"' || c == c2w '\\'
    quoteChar c
        | c == c2w '\"' = byteString "\\\""
        | otherwise     = byteString "\\\\"

hstore :: (HasCallStack, ToHStoreText a, ToHStoreText b) => a -> b -> HStoreBuilder
hstore (toHStoreText -> (HStoreText key)) (toHStoreText -> (HStoreText val)) =
    Comma (char8 '"' `mappend` key `mappend` byteString "\"=>\""
              `mappend` val `mappend` char8 '"')

instance ToField HStoreBuilder where
    toField  Empty    = toField (BS.empty)
    toField (Comma x) = toField (BU.toLazyByteString x)

newtype HStoreList = HStoreList {fromHStoreList :: [(Text,Text)]} deriving (Typeable, Show)

-- | hstore
instance ToHStore HStoreList where
    toHStore (HStoreList xs) = mconcat (map (uncurry hstore) xs)

instance ToField HStoreList where
    toField xs = toField (toHStore xs)

-- | hstore
instance FromField HStoreList where
    fromField f mdat = do
      typ <- typename f
      if typ /= "hstore"
        then returnError Incompatible f ""
        else case mdat of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat ->
                   case P.parseOnly (parseHStore <* P.endOfInput) dat of
                     Left err ->
                         returnError ConversionFailed f err
                     Right (Left err) ->
                         returnError ConversionFailed f "unicode exception" <|>
                           conversionError err
                     Right (Right val) ->
                         return val

newtype HStoreMap  = HStoreMap {fromHStoreMap :: Map Text Text} deriving (Eq, Ord, Typeable, Show)

instance ToHStore HStoreMap where
    toHStore (HStoreMap xs) = Map.foldrWithKey f mempty xs
      where f k v xs = hstore k v `mappend` xs

instance ToField HStoreMap where
    toField xs = toField (toHStore xs)

instance FromField HStoreMap where
    fromField f mdat = convert <$> fromField f mdat
      where convert (HStoreList xs) = HStoreMap (Map.fromList xs)

parseHStoreList :: (HasCallStack) => BS.ByteString -> Either String HStoreList
parseHStoreList dat =
    case P.parseOnly (parseHStore <* P.endOfInput) dat of
      Left err          -> Left (show err)
      Right (Left err)  -> Left (show err)
      Right (Right val) -> Right val

parseHStore :: (HasCallStack) => P.Parser (Either UnicodeException HStoreList)
parseHStore = do
    kvs <- P.sepBy' (skipWhiteSpace *> parseHStoreKeyVal)
                    (skipWhiteSpace *> P.word8 (c2w ','))
    return $ (HStoreList . (DL.foldl' removeNull [])) <$> sequence kvs
    where
      removeNull :: (HasCallStack) => [(Text, Text)] -> (Text, Maybe Text) -> [(Text, Text)]
      removeNull memo (k, Nothing) = memo
      removeNull memo (k, Just v) = memo ++ [(k, v)]

-- NOTE: The Right value is a (Maybe (Text, Text)) to be able to handle
-- key-value pairs where the value is NULL in the DB.
parseHStoreKeyVal :: (HasCallStack) => P.Parser (Either UnicodeException (Text, Maybe Text))
parseHStoreKeyVal = do
  mkey <- parseHStoreText
  case mkey of
    Left err -> return (Left err)
    Right key -> do
      skipWhiteSpace
      _ <- P.string "=>"
      skipWhiteSpace
      mval <- parseHStoreNullableText
      case mval of
        Left  err -> return (Left err)
        Right Nothing -> return (Right (key, Nothing))
        Right (Just val) -> return (Right (key, Just val))


skipWhiteSpace :: (HasCallStack) => P.Parser ()
skipWhiteSpace = P.skipWhile P.isSpace_w8

parseHStoreNullableText :: (HasCallStack) => P.Parser (Either UnicodeException (Maybe Text))
parseHStoreNullableText = nullParser <|> (fmap . fmap) Just parseHStoreText
  where
    nullParser = (P.string "NULL" <|> P.string "null") >> return (Right Nothing)

parseHStoreText :: (HasCallStack) => P.Parser (Either UnicodeException Text)
parseHStoreText = do
  _ <- P.word8 (c2w '"')
  mtexts <- parseHStoreTexts id
  case mtexts of
    Left  err   -> return (Left err)
    Right texts -> do
                     _ <- P.word8 (c2w '"')
                     return (Right (TS.concat texts))

parseHStoreTexts :: (HasCallStack) => ([Text] -> [Text])
                 -> P.Parser (Either UnicodeException [Text])
parseHStoreTexts acc = do
  mchunk <- TS.decodeUtf8' <$> P.takeWhile (not . isSpecialChar)
  case mchunk of
    Left err    -> return (Left err)
    Right chunk ->
        (do
          _ <- P.word8 (c2w '\\')
          c <- TS.singleton . w2c <$> P.satisfy isSpecialChar
          parseHStoreTexts (acc . (chunk:) . (c:))
        ) <|> return (Right (acc [chunk]))
 where
   isSpecialChar c = c == c2w '\\' || c == c2w '"'
