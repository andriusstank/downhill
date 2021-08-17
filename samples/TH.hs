{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Downhill.TH
import Downhill.Grad (HasGrad(Tang))
import Downhill.Linear.Expr (BasicVector(VecBuilder))
import Language.Haskell.TH (runQ)
import Data.Monoid (Sum)
import Data.VectorSpace (AdditiveGroup, VectorSpace)
import GHC.Generics (Generic)

class FooClass a b

data MyRecord = MyRecord
  { myA :: Int,
    myB :: Float
  }

data MyRecordGrad = MyRecordGrad
  { myA :: (Float, Float),
    myB :: Float
  }
  deriving Generic

data MyRecordGradBuilder = MyRecordGradBuilder
  { myA :: VecBuilder (Float, Float),
    myB :: VecBuilder Float
  }

data MyRecordGradBuilder3 = MyRecordGradBuilder3
  { myA :: Sum Int,
    myB :: Sum Float,
    myC :: Sum Float
  }


instD =
  [d|
    instance Semigroup MyRecordGradBuilder where
      MyRecordGradBuilder x1 y1 <> MyRecordGradBuilder x2 y2 = MyRecordGradBuilder (x1<>x2) (y1<>y2)
  |]

data InfixC = Int :^^^ Float

--mkTang ''FooClass
mkTang ''MyRecord
--mkTang ''InfixC

mkRecordSemigroupInstance ''MyRecordGradBuilder
--mkRecordSemigroupInstance ''MyRecordGradBuilder3
instance AdditiveGroup MyRecordGrad
instance VectorSpace MyRecordGrad

--mkBasicVectorInstance ''MyRecordGrad ''MyRecordGradBuilder

main :: IO ()
main = do
  z <- runQ instD
  print z
  return ()
