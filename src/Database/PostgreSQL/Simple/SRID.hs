{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Simple.SRID (
	SRID(..),
	fromLatLon
	) where
import Data.ByteString
import Data.Monoid
import Data.Typeable
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import Blaze.Text (double)
import Blaze.ByteString.Builder (fromByteString, toByteString)

newtype SRID = SRID ByteString deriving (Show, Read, Eq, Typeable)

fromLatLon :: Double -> Double -> SRID
fromLatLon lat lon = SRID $ toByteString $ fromByteString "ST_GeographyFromText('SRID=4326;POINT("
                    `mappend` double lat
                    `mappend` Utf8.fromChar ' '
                    `mappend` double lon
                    `mappend` fromByteString ")')" 