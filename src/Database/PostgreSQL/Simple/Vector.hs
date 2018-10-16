-- | 'query' variants returning 'V.Vector'.
module Database.PostgreSQL.Simple.Vector where

import           Database.PostgreSQL.Simple (Connection, formatQuery, formatMany)
import           Database.PostgreSQL.Simple.FromRow (FromRow(..))
import           Database.PostgreSQL.Simple.ToRow (ToRow(..))
import           Database.PostgreSQL.Simple.Internal (RowParser, exec)
import           Database.PostgreSQL.Simple.Internal.PQResultUtils
import           Database.PostgreSQL.Simple.Types ( Query (..) )

import qualified Data.Vector as V

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO (V.Vector r)
query = queryWith fromRow

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow r) => Connection -> Query -> IO (V.Vector r)
query_ = queryWith_ fromRow

-- | A version of 'query' taking parser as argument
queryWith :: ToRow q => RowParser r -> Connection -> Query -> q -> IO (V.Vector r)
queryWith parser conn template qs = do
  result <- exec conn =<< formatQuery conn template qs
  finishQueryWithV parser conn template result

-- | A version of 'query_' taking parser as argument
queryWith_ :: RowParser r -> Connection -> Query -> IO (V.Vector r)
queryWith_ parser conn q@(Query que) = do
  result <- exec conn que
  finishQueryWithV parser conn q result

-- | Execute @INSERT ... RETURNING@, @UPDATE ... RETURNING@, or other SQL
-- query that accepts multi-row input and is expected to return results.
returning :: (ToRow q, FromRow r) => Connection -> Query -> [q] -> IO (V.Vector r)
returning = returningWith fromRow

-- | A version of 'returning' taking parser as argument
returningWith :: (ToRow q) => RowParser r -> Connection -> Query -> [q] -> IO (V.Vector r)
returningWith _ _ _ [] = return V.empty
returningWith parser conn q qs = do
  result <- exec conn =<< formatMany conn q qs
  finishQueryWithV parser conn q result
