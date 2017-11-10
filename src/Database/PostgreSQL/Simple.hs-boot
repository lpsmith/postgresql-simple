module Database.PostgreSQL.Simple
     ( Connection
     , Query
     , query
     , query_
     , execute
     , execute_
     , executeMany
     ) where

import Data.Int(Int64)
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.Types
import {-# SOURCE #-} Database.PostgreSQL.Simple.FromRow
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToRow
import GHC.Stack

query :: (HasCallStack, ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]

query_ :: (HasCallStack, FromRow r) => Connection -> Query -> IO [r]

execute :: (HasCallStack, ToRow q) => Connection -> Query -> q -> IO Int64

executeMany :: (HasCallStack, ToRow q) => Connection -> Query -> [q] -> IO Int64
