{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Record(recordTest) where

import Downhill.BVar.Traversable (TraversableVar (TraversableVar), backpropTraversable, splitTraversable)
import Downhill.BVar (BVar (BVar), backprop, var)
import Downhill.Grad (HasGrad (Grad))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

data MyRecord a = MyRecord
  { memberPair :: (a, a),
    memberList :: [a]
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

deriving via (TraversableVar MyRecord a) instance HasGrad a => HasGrad (MyRecord a)

test_r :: MyRecord Integer
test_r = MyRecord (10, 11) [12, 13, 14]

test_dr :: MyRecord (Integer, Integer)
test_dr =
  MyRecord
    ((10, 2), (11, 3))
    [(12, 5), (13, 5), (14, 5)]

test_fun :: Num a => MyRecord a -> a
test_fun (MyRecord (x, y) zs) = 2 * x + 3 * y + 5 * sum zs

type MyGrad a = Grad (MyRecord a)

foo :: MyRecord (Integer, Integer)
foo = backpropTraversable 1 (,) test_fun test_r
  where
    x :: BVar (MyGrad Integer) (MyRecord Integer)
    x = var test_r
    x' :: MyRecord (BVar (MyGrad Integer) Integer)
    x' = splitTraversable x
    y :: BVar (MyGrad Integer) Integer
    y = test_fun x'

recordTest :: TestTree
recordTest = testCase "Traverse record" (foo @?= test_dr)

main :: IO ()
main = return ()
