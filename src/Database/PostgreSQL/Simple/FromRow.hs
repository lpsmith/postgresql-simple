{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Database.PostgreSQL.Simple.FromRow
     ( FromRow(..)
     , RowParser
     , field
     , numFieldsRemaining
     ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (SomeException(..), throw)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Result (ResultError(..), Result(..))
import Database.PostgreSQL.Simple.Types (Only(..))
import Database.PostgreSQL.Simple.Ok
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.FromField

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.Vector ((!))

import System.IO.Unsafe ( unsafePerformIO )

class FromRow a where
    fromRow :: RowParser a

field :: FromField a => RowParser a
field = RP $ do
    Row{..} <- ask
    column <- lift get
    lift (put (column + 1))
    let typename = typenames ! (\(PQ.Col x) -> fromIntegral x) column
        result = rowresult
        field = Field{..}
    if (column > nfields rowresult) then lift (lift (Errors [])) else return ()
    lift (lift (fromField field (getvalue result row column)))

numFieldsRemaining :: RowParser Int
numFieldsRemaining = RP $ do
    Row{..} <- ask
    column <- lift get
    return $! (\(PQ.Col x) -> fromIntegral x) (nfields rowresult - column)
    

instance (FromField a) => FromRow (Only a) where
    fromRow = do
        !a <- field
        return (Only a)

instance (FromField a, FromField b) => FromRow (a,b) where
    fromRow = do
        !a <- field
        !b <- field
        return (a,b)

instance (FromField a, FromField b, FromField c) => FromRow (a,b,c) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        return (a,b,c)

instance (FromField a, FromField b, FromField c, FromField d) =>
    FromRow (a,b,c,d) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        return (a,b,c,d)

instance (FromField a, FromField b, FromField c, FromField d, FromField e) =>
    FromRow (a,b,c,d,e) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        !e <- field
        return (a,b,c,d,e)

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f) =>
    FromRow (a,b,c,d,e,f) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        !e <- field
        !f <- field
        return (a,b,c,d,e,f)

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g) =>
    FromRow (a,b,c,d,e,f,g) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        !e <- field
        !f <- field
        !g <- field
        return (a,b,c,d,e,f,g)

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h) =>
    FromRow (a,b,c,d,e,f,g,h) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        !e <- field
        !f <- field
        !g <- field
        !h <- field
        return (a,b,c,d,e,f,g,h)

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i) =>
    FromRow (a,b,c,d,e,f,g,h,i) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        !e <- field
        !f <- field
        !g <- field
        !h <- field
        !i <- field
        return (a,b,c,d,e,f,g,h,i)

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j) =>
    FromRow (a,b,c,d,e,f,g,h,i,j) where
    fromRow = do
        !a <- field
        !b <- field
        !c <- field
        !d <- field
        !e <- field
        !f <- field
        !g <- field
        !h <- field
        !i <- field
        !j <- field
        return (a,b,c,d,e,f,g,h,i,j)

instance FromField a => FromRow [a] where
    fromRow = do
      n <- numFieldsRemaining
      replicateM n field
