{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures #-}
-- {-# OPTIONS_GHC -dsuppress-uniques #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
module Main where

import           Test.Inspection
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Database.PostgreSQL.LibPQ                  as PQ
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.TypeInfo        as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI

-------------------------------------------------------------------------------
-- Inspection tests
-------------------------------------------------------------------------------

-- # doesn't work :(
#define TH_MKCOMPATS3(a,b,c) $(mkCompats [TI.a,TI.b,TI.c])
#define IN_MKCOMPATS3(a,b,c) (eq TI.a \/ eq TI.b \/ eq TI.c)

#define TH_INLINETYPOID(n) eq $(inlineTypoid TI.n)
#define IN_INLINETYPOID(n) eq TI.n

-- eta-expansion is required
lhs01, rhs01 :: PQ.Oid -> Bool
lhs01 = TH_MKCOMPATS3(name,text,char)
rhs01 = IN_MKCOMPATS3(nameOid,textOid,charOid)

lhs02, rhs02 :: PQ.Oid -> Bool
lhs02 = TH_INLINETYPOID(name)
rhs02 = IN_INLINETYPOID(nameOid)

eq :: PQ.Oid -> PQ.Oid -> Bool
eq = (==)
{-# INLINE eq #-}

infixr 2 \/
(\/) :: (PQ.Oid -> Bool)
     -> (PQ.Oid -> Bool)
     -> (PQ.Oid -> Bool)
f \/ g = \x -> f x || g x
{-# INLINE (\/) #-}

inspectionTests :: TestTree
inspectionTests = testGroup "inspection"
    [ testCase "mkCompats" $
        assertSuccess $(inspectTest $ 'lhs01 === 'rhs01)
    
    -- byteaOid isn't inlined?
    , testCase "inlineTypoid" $
#if __GLASGOW_HASKELL__ >= 808
        assertSuccess
#else
        assertFailure'
#endif
          $(inspectTest $ 'lhs02 ==- 'rhs02)
    ]

assertSuccess :: Result -> IO ()
assertSuccess (Success _)   = return ()
assertSuccess (Failure err) = assertFailure err

assertFailure' :: Result -> IO ()
assertFailure' (Success err) = assertFailure err
assertFailure' (Failure _)   = return ()

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ inspectionTests
    ]
