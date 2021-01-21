import Diff(bvarValue, BVarS, var, constant, backpropS)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import qualified Test.Tasty as Tasty

basicTests = testGroup "Basic tests"
  [ testCase "Derivative of constant == 0" $ testConstant
  , testCase "Derivative of identity == 1" $ testIdentity
  , testCase "Derivative of simple polynomial" $ testPoly
  ]
  where testConstant = backpropS (constant 3 :: BVarS Integer) @?= 0
        testIdentity = backpropS (var 3 :: BVarS Integer) @?= 1 
        testPoly =
            let x = var 5 :: BVarS Integer
            in backpropS ((2+3*x) * (5+7*x)) @?= 29 + 42 * bvarValue x

tests :: TestTree
tests = testGroup "Tests" [basicTests]

main = defaultMain tests
