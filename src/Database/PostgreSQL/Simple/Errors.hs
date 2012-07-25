{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.PostgreSQL.Simple.Errors
       ( ConstraintViolation(..)
       , constraintViolation
       , constraintViolationE
       , catchViolation
       )
       where

import Prelude hiding (catch)

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class

import           Data.Attoparsec.Char8
import           Data.ByteString(ByteString)
import           Data.Typeable

import           Database.PostgreSQL.Simple.Internal


data ConstraintViolation
   = NotNullViolation ByteString
   | ForeignKeyViolation ByteString ByteString
   | UniqueViolation ByteString
   | CheckViolation ByteString ByteString
   deriving (Show, Eq, Ord, Typeable)


-- | Tries to convert 'SqlError' to 'ConstrainViolation', checks sqlState and
-- succeedes only if able to parse sqlErrorMsg. May depend on postgres language
-- settings.
--
-- > createUser = catchJust constraintViolation catcher $ execute conn ...
-- >   where
-- >     catcher UniqueViolation "user_login_key" = ...
-- >     catcher _ = ...
constraintViolation :: SqlError -> Maybe ConstraintViolation
constraintViolation e =
  case sqlState e of
    "23502" -> parseMaybe parseQ1 msg >>= Just . NotNullViolation
    "23503" -> parseMaybe parseQ2 msg >>= Just . uncurry ForeignKeyViolation
    "23505" -> parseMaybe parseQ1 msg >>= Just . UniqueViolation
    "23514" -> parseMaybe parseQ2 msg >>= Just . uncurry CheckViolation
    _ -> Nothing
  where msg = sqlErrorMsg e


-- | Like constraintViolation, but also packs original SqlError.
--
-- > createUser = catchJust constraintViolationE catcher $ execute conn ...
-- >   where
-- >     catcher (_, UniqueViolation "user_login_key") = ...
-- >     catcher (e, _) = throw e
--
constraintViolationE :: SqlError -> Maybe (SqlError, ConstraintViolation)
constraintViolationE e = fmap ((,) e) $ constraintViolation e

-- | Catches SqlError, tries to convert to ConstraintViolation, re-throws
-- on fail. Unlike catchJust, provides alternative interface to catchJust
--
-- > createUser = catchViolation catcher $ execute conn ...
-- >   where
-- >     catcher _ (UniqueViolation "user_login_key") = ...
-- >     catcher e _ = throw e
catchViolation :: (SqlError -> ConstraintViolation -> IO a) -> IO a -> IO a
catchViolation f m = catch m
                     (\e -> maybe (throw e) (f e) $ constraintViolation e)

-- Parsers just try to extract quoted strings from error messages, number
-- of quoted strings depend on error type.
scanTillQuote :: Parser ByteString
scanTillQuote = scan False go
  where go True _ = Just False -- escaped character
        go False '"' = Nothing -- end parse
        go False '\\' = Just True -- nest one is escaped
        go _ _ = Just False

parseQ1 :: Parser ByteString
parseQ1 = scanTillQuote *> char '"' *> scanTillQuote <* char '"'

parseQ2 :: Parser (ByteString, ByteString)
parseQ2 = (,) <$> parseQ1 <*> parseQ1

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe p b = either (const Nothing) Just $ parseOnly p b
