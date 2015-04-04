module Database.PostgreSQL.Simple.ToField where

import Database.PostgreSQL.Simple.Types
import Data.ByteString.Builder(Builder)
import Data.ByteString(ByteString)

-- | How to render an element when substituting it into a query.
data Action =
    Plain Builder
    -- ^ Render without escaping or quoting. Use for non-text types
    -- such as numbers, when you are /certain/ that they will not
    -- introduce formatting vulnerabilities via use of characters such
    -- as spaces or \"@'@\".
  | Escape ByteString
    -- ^ Escape and enclose in quotes before substituting. Use for all
    -- text-like types, and anything else that may contain unsafe
    -- characters when rendered.
  | EscapeByteA ByteString
    -- ^ Escape binary data for use as a @bytea@ literal.  Include surrounding
    -- quotes.  This is used by the 'Binary' newtype wrapper.
  | EscapeIdentifier ByteString
    -- ^ Escape before substituting. Use for all sql identifiers like
    -- table, column names, etc. This is used by the 'Identifier' newtype
    -- wrapper.
  | Many [Action]
    -- ^ Concatenate a series of rendering actions.

class ToField a

instance ToField Oid
