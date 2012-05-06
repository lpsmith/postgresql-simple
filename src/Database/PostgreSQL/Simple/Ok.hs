{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Database.PostgreSQL.Simple.Ok
-- Copyright   :  (c) 2011-2012 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  leon@melding-monads.com
-- Stability   :  experimental
--
-- The 'Ok' type is a simple error handler,  basically equivalent to
-- @Either [SomeException]@.   This type (without the list) was used to
-- handle conversion errors in early versions of postgresql-simple.
--
-- One of the primary reasons why this type  was introduced is that
-- @Either SomeException@ had not been provided an instance for 'Alternative',
-- and it would have been a bad idea to provide an orphaned instance for a
-- commonly-used type and typeclass included in @base@.
--
-- Extending the failure case to a list of 'SomeException's enables a
-- more sensible 'Alternative' instance definitions:   '<|>' concatinates
-- the list of exceptions when both cases fail,  and 'empty' is defined as
-- 'Errors []'.   Though '<|>' one could pick one of two exceptions, and
-- throw away the other,  and have 'empty' provide a generic exception,
-- this avoids cases where 'empty' overrides a more informative exception
-- and allows you to see all the different ways your computation has failed.
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.Ok where

import Control.Applicative
import Control.Exception
import Control.Monad(MonadPlus(..))
import Data.Typeable

-- FIXME:   [SomeException] should probably be something else,  maybe
--          a difference list (or a tree?)

data Ok a = Errors [SomeException] | Ok !a
    deriving(Show, Typeable, Functor)

-- | Two 'Errors' cases are considered equal, regardless of what the
--   list of exceptions looks like.

instance Eq a => Eq (Ok a) where
    Errors _ == Errors _  = True
    Ok  a    == Ok  b     = a == b
    _        == _         = False

instance Applicative Ok where
    pure = Ok

    Errors es <*> _ = Errors es
    _ <*> Errors es = Errors es
    Ok f <*> Ok a   = Ok (f a)

instance Alternative Ok where
    empty = Errors []

    a@(Ok _)  <|> _         = a
    Errors _  <|> b@(Ok _)  = b
    Errors as <|> Errors bs = Errors (as ++ bs)

instance MonadPlus Ok where
    mzero = empty
    mplus = (<|>)

instance Monad Ok where
    return = Ok

    Errors es >>= _ = Errors es
    Ok a      >>= f = f a
    -- TODO:  add a definition for "fail",  akin to

    -- fail str = Errors [SomeException (error str)]

    -- but *correct*,  as this will throw an exception if you try to
    -- examine the exception.

-- | a way to reify a list of exceptions into a single exception

newtype ManyErrors = ManyErrors [SomeException]
   deriving (Show, Typeable)

instance Exception ManyErrors
