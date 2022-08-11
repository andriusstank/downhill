{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.Traversable(recordTest) where

import Downhill.BVar.Traversable (TraversableVar (TraversableVar), backpropTraversable, splitTraversable)
import Downhill.BVar (BVar (BVar), backprop, var)
import Downhill.Grad (Manifold (Grad), HasGrad)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

data MyRecord a = MyRecord
  { memberPair :: (a, a),
    memberList :: [a]
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

deriving via (TraversableVar MyRecord a) instance Manifold a => Manifold (MyRecord a)

test_r :: MyRecord Integer
test_r = MyRecord (10, 11) [12, 13, 14]

expectedResult :: MyRecord (Integer, Integer)
expectedResult =
  MyRecord
    ((10, 2), (11, 3))
    [(12, 5), (13, 5), (14, 5)]

test_fun :: Num a => MyRecord a -> a
test_fun (MyRecord (x, y) zs) = 2 * x + 3 * y + 5 * sum zs

type MyGrad a = Grad (MyRecord a)

actualResult :: MyRecord (Integer, Integer)
actualResult = backpropTraversable 1 (,) test_fun test_r
  where
    x :: BVar (MyGrad Integer) (MyRecord Integer)
    x = var test_r
    x' :: MyRecord (BVar (MyGrad Integer) Integer)
    x' = splitTraversable x
    y :: BVar (MyGrad Integer) Integer
    y = test_fun x'

recordTest :: TestTree
recordTest = testCase "Traverse record" (actualResult @?= expectedResult)

main :: IO ()
main = return ()
