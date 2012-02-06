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
-- of writing big sql statements in Haskell source code.  For example:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > query conn [sql| SELECT column_a, column_b
-- >                    FROM table1 NATURAL JOIN table2
-- >                   WHERE ? <= time AND time < ?
-- >                     AND name LIKE ?
-- >                   ORDER BY size DESC
-- >                   LIMIT 100                        |]
-- >            (beginTime,endTime,string)
--
-- This quasiquoter attempts to mimimize whitespace;  otherwise the
-- above query would consist of approximately half whitespace when sent
-- to the database backend.
--
-- The implementation of the whitespace reducer is currently incomplete.
-- Thus it can mess up your syntax in cases where whitespace should be
-- preserved as-is.  It does preserve whitespace inside standard SQL string
-- literals.  But it can get confused by the non-standard PostgreSQL string
-- literal syntax (which is the default setting in PostgreSQL 8 and below),
-- the extended escape string syntax,  and other similar constructs.
--
-- Of course, this caveat only applies to text written inside the SQL
-- quasiquoter; whitespace reduction is a compile-time computation and
-- thus will not touch the @string@ parameter above,  which is a run-time
-- value.
--
-- Also note that this will not work if the substring @|]@ is contained
-- in the query.

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
