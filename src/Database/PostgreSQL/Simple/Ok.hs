{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}

module Database.PostgreSQL.Simple.Ok where

import Control.Applicative
import Control.Exception
import Data.Typeable

-- FIXME:   [SomeException] should probably be a difference list

data Ok a = Errors [SomeException] | Ok !a
    deriving(Show, Typeable, Functor)

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

instance Monad Ok where
    return = Ok

    Errors es >>= _ = Errors es
    Ok a      >>= f = f a
    -- TODO:  add a definition for "fail",  akin to

    -- fail str = Errors [SomeException (error str)]

    -- but *correct*,  as this will throw an exception if you try to
    -- examine the exception.
