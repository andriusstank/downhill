{- Why automatic differentiation with records is so complicated in Haskell, while
the same thing is so easy in Python with JAX?

Haskell counterpart of pytree is Traversable. Differentiating traversables
is easy. Of course, it is only applicable if all variables are of the same type.
-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where

import Downhill.BVar.Traversable (backpropTraversable_GradOnly, TraversableVar)
import Downhill.BVar (BVar)
import Downhill.Grad (HasGrad)

data MyRecord a = MyRecord
  { mrPair :: (a, a),
    mrList :: [a]
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

deriving via (TraversableVar MyRecord a) instance HasGrad a => HasGrad (MyRecord a)

-- This version is less general than _myRecordGrad'. It shows that
-- type of function to be differentiated does not need to mention BVar.
{-# ANN myRecordGrad ("HLint: Eta reduce" :: String) #-}
myRecordGrad :: (forall a. Num a => MyRecord a -> a) -> MyRecord Double -> MyRecord Double
myRecordGrad f = backpropTraversable_GradOnly 1 f

_myRecordGrad' :: (forall r. MyRecord (BVar r Double) -> BVar r Double) ->  MyRecord Double -> MyRecord Double
_myRecordGrad' = backpropTraversable_GradOnly 1

sqr :: Num a => a -> a
sqr x = x ^ (2 :: Int)

main :: IO ()
main = print (myRecordGrad fun record)
  where 
    record :: MyRecord Double
    record = MyRecord (10, 11) [12, 13, 14]
    fun :: Num a => MyRecord a -> a
    fun (MyRecord (x, y) zs) = 2 * x + 3 * y + sum (map sqr zs)
