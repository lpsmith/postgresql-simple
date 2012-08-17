{-# LANGUAGE TemplateHaskell #-}

module StringsQQ (strings, longstring, typenames) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Char (toUpper)

strings = QuasiQuoter { quotePat = undefined,
                        quoteType = undefined,
                        quoteExp = stringsExp,
                        quoteDec = undefined  }

-- stringE = return . LitE . StringL

stringsExp :: String -> Q Exp
stringsExp = foldr (\x xs -> [e| $(x) : $(xs) |]) [e| [] |]
           . map delta . filter (not . null) . map words . lines
  where delta [(x:xs)] = [e| ( $(stringE $ x:xs )
                             , $(stringE $ toUpper x : xs ) ) |]
        delta [xs,ys]  = [e| ( $(stringE xs) , $(stringE ys) ) |]

longstring = QuasiQuoter { quotePat = undefined,
                           quoteType = undefined,
                           quoteExp = longstringExp,
                           quoteDec = undefined }

longstringExp ('\n':xs) = stringE xs
longstringExp xs        = stringE xs

typenames = QuasiQuoter { quotePat = undefined,
                          quoteType = undefined,
                          quoteExp = typenamesExp,
                          quoteDec = undefined  }

typenamesExp :: String -> Q Exp
typenamesExp = foldr (\x xs -> [e| $(x) : $(xs) |]) [e| [] |]
             . map delta . filter (not . null) . map words . lines
  where delta [xs]     = delta [xs,xs]
        delta [xs,ys]  = [e| ( $(stringE xs) , $(stringE ys) ) |]
