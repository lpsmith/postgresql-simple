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
import           Data.Semigroup
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

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
                Comma c -> c

toLazyByteString :: HStoreBuilder -> BL.ByteString
toLazyByteString x = case x of
                       Empty -> BL.empty
                       Comma c -> BU.toLazyByteString c

instance Semigroup HStoreBuilder where
    Empty   <> x = x
    Comma a <> x
        = Comma (a `mappend` case x of
                               Empty   -> mempty
                               Comma b -> char8 ',' `mappend` b)

instance Monoid HStoreBuilder where
    mempty = Empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

class ToHStoreText a where
  toHStoreText :: a -> HStoreText

-- | Represents escape text, ready to be the key or value to a hstore value
newtype HStoreText = HStoreText Builder deriving (Typeable, Semigroup, Monoid)

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

escapeAppend :: BS.ByteString -> Builder -> Builder
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

hstore :: (ToHStoreText a, ToHStoreText b) => a -> b -> HStoreBuilder
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
      where f k v xs' = hstore k v `mappend` xs'

instance ToField HStoreMap where
    toField xs = toField (toHStore xs)

instance FromField HStoreMap where
    fromField f mdat = convert <$> fromField f mdat
      where convert (HStoreList xs) = HStoreMap (Map.fromList xs)

parseHStoreList :: BS.ByteString -> Either String HStoreList
parseHStoreList dat =
    case P.parseOnly (parseHStore <* P.endOfInput) dat of
      Left err          -> Left (show err)
      Right (Left err)  -> Left (show err)
      Right (Right val) -> Right val

parseHStore :: P.Parser (Either UnicodeException HStoreList)
parseHStore = do
    kvs <- P.sepBy' (skipWhiteSpace *> parseHStoreKeyVal)
                    (skipWhiteSpace *> P.word8 (c2w ','))
    return $ HStoreList <$> sequence kvs

parseHStoreKeyVal :: P.Parser (Either UnicodeException (Text,Text))
parseHStoreKeyVal = do
  mkey <- parseHStoreText
  case mkey of
    Left err -> return (Left err)
    Right key -> do
      skipWhiteSpace
      _ <- P.string "=>"
      skipWhiteSpace
      mval <- parseHStoreText
      case mval of
        Left  err -> return (Left err)
        Right val -> return (Right (key,val))


skipWhiteSpace :: P.Parser ()
skipWhiteSpace = P.skipWhile P.isSpace_w8

parseHStoreText :: P.Parser (Either UnicodeException Text)
parseHStoreText = do
  _ <- P.word8 (c2w '"')
  mtexts <- parseHStoreTexts id
  case mtexts of
    Left  err   -> return (Left err)
    Right texts -> do
                     _ <- P.word8 (c2w '"')
                     return (Right (TS.concat texts))

parseHStoreTexts :: ([Text] -> [Text])
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
