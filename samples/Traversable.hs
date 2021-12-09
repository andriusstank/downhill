{- Why automatic differentiation with records is so complicated in Haskell, while
the same thing is so easy in Python with JAX?

Haskell counterpart of pytree is Traversable. 

-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Main(main) where

import Downhill.BVar.Traversable (TraversableVar (TraversableVar), backpropTraversable, splitTraversable, backpropTraversable_GradOnly)
import Downhill.BVar (BVar (BVar), backprop, var)
import Downhill.Grad (HasGrad (Grad))

data MyRecord a = MyRecord
  { mrPair :: (a, a),
    mrList :: [a]
  }
  deriving (Eq, Functor, Foldable, Traversable, Show)

deriving via (TraversableVar MyRecord a) instance HasGrad a => HasGrad (MyRecord a)

myRecordGrad :: (forall a. Num a => MyRecord a -> a) -> MyRecord Double -> MyRecord Double
myRecordGrad = backpropTraversable_GradOnly 1

myRecordGrad' :: (forall r. MyRecord (BVar r Double) -> BVar r Double) ->  MyRecord Double -> MyRecord Double
myRecordGrad' = backpropTraversable_GradOnly 1

main :: IO ()
main = print (myRecordGrad fun record)
  where 
    record :: MyRecord Double
    record = MyRecord (10, 11) [12, 13, 14]
    fun :: Num a => MyRecord a -> a
    fun (MyRecord (x, y) zs) = 2 * x + 3 * y + sum (map (^2) zs)
