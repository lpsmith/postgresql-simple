{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.Errors
-- Copyright:   (c) 2012-2013 Leonid Onokhov, Joey Adams
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- | Module for parsing errors from postgresql error messages.
--  Currently only parses integrity violation errors (class 23).
--
-- /Note: Success of parsing may depend on language settings./
----------------------------------------------------------
module Database.PostgreSQL.Simple.Errors
       ( ConstraintViolation(..)
       , constraintViolation
       , constraintViolationE
       , catchViolation
       , isSerializationError
       , isNoActiveTransactionError
       , isFailedTransactionError
       )
       where

import Control.Applicative
import Control.Exception as E

import Data.Attoparsec.ByteString.Char8
import Data.ByteString       (ByteString)
import Data.Typeable

import Database.PostgreSQL.Simple.Internal

-- Examples of parsed error messages
--
-- `ERROR:  new row for relation "users" violates check
-- constraint "user_kind_check"`
--
-- `ERROR:  insert or update on table "user_group_map" violates foreign key
--  constraint "user_id"`
--
-- `ERROR: null value in column "login" violates not-null constraint`
--
-- `ERROR: duplicate key value violates unique constraint "users_login_key"`

data ConstraintViolation
   = NotNullViolation ByteString
   -- ^ The field is a column name
   | ForeignKeyViolation ByteString ByteString
   -- ^ Table name and name of violated constraint
   | UniqueViolation ByteString
   -- ^ Name of violated constraint
   | CheckViolation ByteString ByteString
   -- ^ Relation name (usually table), constraint name
   | ExclusionViolation ByteString
   -- ^ Name of the exclusion violation constraint
   deriving (Show, Eq, Ord, Typeable)

-- Default instance should be enough
instance Exception ConstraintViolation


-- | Tries to convert 'SqlError' to 'ConstrainViolation', checks sqlState and
-- succeedes only if able to parse sqlErrorMsg.
--
-- > createUser = handleJust constraintViolation handler $ execute conn ...
-- >   where
-- >     handler (UniqueViolation "user_login_key") = ...
-- >     handler _ = ...
constraintViolation :: SqlError -> Maybe ConstraintViolation
constraintViolation e =
  case sqlState e of
    "23502" -> NotNullViolation <$> parseMaybe parseQ1 msg
    "23503" -> uncurry ForeignKeyViolation <$> parseMaybe parseQ2 msg
    "23505" -> UniqueViolation <$> parseMaybe parseQ1 msg
    "23514" -> uncurry CheckViolation <$> parseMaybe parseQ2 msg
    "23P01" -> ExclusionViolation <$> parseMaybe parseQ1 msg
    _ -> Nothing
  where msg = sqlErrorMsg e


-- | Like constraintViolation, but also packs original SqlError.
--
-- > createUser = handleJust constraintViolationE handler $ execute conn ...
-- >   where
-- >     handler (_, UniqueViolation "user_login_key") = ...
-- >     handler (e, _) = throwIO e
--
constraintViolationE :: SqlError -> Maybe (SqlError, ConstraintViolation)
constraintViolationE e = fmap ((,) e) $ constraintViolation e

-- | Catches SqlError, tries to convert to ConstraintViolation, re-throws
-- on fail. Provides alternative interface to 'E.handleJust'
--
-- > createUser = catchViolation catcher $ execute conn ...
-- >   where
-- >     catcher _ (UniqueViolation "user_login_key") = ...
-- >     catcher e _ = throwIO e
catchViolation :: (SqlError -> ConstraintViolation -> IO a) -> IO a -> IO a
catchViolation f m = E.catch m
                     (\e -> maybe (throwIO e) (f e) $ constraintViolation e)

-- Parsers just try to extract quoted strings from error messages, number
-- of quoted strings depend on error type.
scanTillQuote :: Parser ByteString
scanTillQuote = scan False go
  where go True _ = Just False -- escaped character
        go False '"' = Nothing -- end parse
        go False '\\' = Just True -- next one is escaped
        go _ _ = Just False

parseQ1 :: Parser ByteString
parseQ1 = scanTillQuote *> char '"' *> scanTillQuote <* char '"'

parseQ2 :: Parser (ByteString, ByteString)
parseQ2 = (,) <$> parseQ1 <*> parseQ1

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe p b = either (const Nothing) Just $ parseOnly p b

------------------------------------------------------------------------
-- Error predicates
--
-- https://www.postgresql.org/docs/9.5/static/errcodes-appendix.html

isSerializationError :: SqlError -> Bool
isSerializationError = isSqlState "40001"

isNoActiveTransactionError :: SqlError -> Bool
isNoActiveTransactionError = isSqlState "25P01"

isFailedTransactionError :: SqlError -> Bool
isFailedTransactionError = isSqlState "25P02"

isSqlState :: ByteString -> SqlError -> Bool
isSqlState s SqlError{..} = sqlState == s
