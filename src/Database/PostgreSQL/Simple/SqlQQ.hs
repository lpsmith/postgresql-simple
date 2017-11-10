{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module:      Database.PostgreSQL.Simple.SqlQQ
-- Copyright:   (c) 2011-2012 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
------------------------------------------------------------------------------

module Database.PostgreSQL.Simple.SqlQQ (sql) where
import Database.PostgreSQL.Simple.Types (Query)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Data.String
import GHC.Stack

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
-- This quasiquoter returns a literal string expression of type 'Query',
-- and attempts to mimimize whitespace;  otherwise the above query would
-- consist of approximately half whitespace when sent to the database
-- backend.  It also recognizes and strips out standard sql comments "--".
--
-- The implementation of the whitespace reducer is currently incomplete.
-- Thus it can mess up your syntax in cases where whitespace should be
-- preserved as-is.  It does preserve whitespace inside standard SQL string
-- literals.  But it can get confused by the non-standard PostgreSQL string
-- literal syntax (which is the default setting in PostgreSQL 8 and below),
-- the extended escape string syntax,  quoted identifiers,  and other similar
-- constructs.
--
-- Of course, this caveat only applies to text written inside the SQL
-- quasiquoter; whitespace reduction is a compile-time computation and
-- thus will not touch the @string@ parameter above,  which is a run-time
-- value.
--
-- Also note that this will not work if the substring @|]@ is contained
-- in the query.

sql :: (HasCallStack) => QuasiQuoter
sql = QuasiQuoter
    { quotePat  = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                        \ quasiquoter used in pattern context"
    , quoteType = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                        \ quasiquoter used in type context"
    , quoteExp  = sqlExp
    , quoteDec  = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                        \ quasiquoter used in declaration context"
    }

sqlExp :: (HasCallStack) => String -> Q Exp
sqlExp = appE [| fromString :: String -> Query |] . stringE . minimizeSpace

minimizeSpace :: (HasCallStack) => String -> String
minimizeSpace = drop 1 . reduceSpace
  where
    needsReduced []          = False
    needsReduced ('-':'-':_) = True
    needsReduced (x:_)       = isSpace x

    reduceSpace xs =
        case dropWhile isSpace xs of
          [] -> []
          ('-':'-':ys) -> reduceSpace (dropWhile (/= '\n') ys)
          ys -> ' ' : insql ys

    insql ('\'':xs)            = '\'' : instring xs
    insql xs | needsReduced xs = reduceSpace xs
    insql (x:xs)               = x : insql xs
    insql []                   = []

    instring ('\'':'\'':xs) = '\'':'\'': instring xs
    instring ('\'':xs)      = '\'': insql xs
    instring (x:xs)         = x : instring xs
    instring []             = error "Database.PostgreSQL.Simple.SqlQQ.sql:\
                                    \ string literal not terminated"
