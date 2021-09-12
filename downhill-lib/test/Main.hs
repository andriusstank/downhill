import Downhill.BVar(bvarValue)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import qualified Test.Tasty as Tasty
import Downhill.BVar.Num (NumBVar(..), backpropNum, constant, var, numbvarValue, AsNum)
import Record(recordTest)
import TH (thTest)

basicTests = testGroup "Basic tests"
  [ testCase "Derivative of constant == 0" testConstant
  , testCase "Derivative of identity == 1" testIdentity
  , testCase "Derivative of simple polynomial" testPoly
  ]
  where testConstant = backpropNum (constant 3 :: NumBVar Integer) @?= 0
        testIdentity = backpropNum (var 3 :: NumBVar Integer) @?= 1 
        testPoly =
            let x = var 5 :: NumBVar Integer
                y = 3*x :: NumBVar Integer
            in backpropNum ((2+3*x) * (5+7*x)) @?= 29 + 42 * numbvarValue x

tests :: TestTree
tests = testGroup "Tests" [basicTests, recordTest, thTest]

main = defaultMain tests
