------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.SqlQQ
-- Copyright:   (c) 2011 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.SqlQQ (sql) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char

-- | 'sql' is a quasiquoter that eases the syntactic burden
-- of writing big sql statements in Haskell source code.  It attempts
-- to minimize whitespace.  Note that this implementation is incomplete
-- and can mess up your syntax;  it only really understands standard
-- sql string literals (default in PostgreSQL 9) and not the extended
-- escape syntax or other situations where white space should be
-- preserved as is.

sql :: QuasiQuoter
sql = QuasiQuoter
    { quotePat  = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                        \ quasiquoter used in pattern context"
    , quoteType = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                        \ quasiquoter used in type context"
    , quoteExp  = sqlExp
    , quoteDec  = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                        \ quasiquoter used in declaration context"
    }

sqlExp :: String -> Q Exp
sqlExp = stringE . outstring . dropSpace
  where
    dropSpace = dropWhile isSpace

    outstring ('\'':xs) = '\'' : instring xs
    outstring (x:xs) | isSpace x = case dropSpace xs of
                                     [] -> []
                                     ys -> ' ' : outstring ys
                     | otherwise = x : outstring xs
    outstring [] = []

    instring ('\'':'\'':xs) = '\'':'\'': instring xs
    instring ('\'':xs)      = '\'': outstring xs
    instring (x:xs)         = x : instring xs
    instring []             = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                                    \ string literal not terminated"
