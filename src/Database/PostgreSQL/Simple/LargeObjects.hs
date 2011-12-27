-----------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.LargeObjects
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
--
-----------------------------------------------------------------------------

module Database.PostgreSQL.Simple.LargeObjects
     ( loImport
     , loExport
     , Oid(..)
     ) where

import           Database.PostgreSQL.LibPQ (Oid(..))
import qualified Database.PostgreSQL.LibPQ as PQ
import           Database.PostgreSQL.Simple.Internal
import           Foreign.C.Types(CInt)

loImport :: Connection -> FilePath -> IO (Maybe Oid)
loImport conn path = withConnection conn $ \c -> PQ.loImport c path

loExport :: Connection -> Oid -> FilePath -> IO (Maybe ())
loExport conn oid path = withConnection conn $ \c -> PQ.loExport c oid path
