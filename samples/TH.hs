{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Downhill.TH
import Downhill.Grad (HasGrad(Tang))
import Downhill.Linear.Expr (BasicVector(VecBuilder))
import Language.Haskell.TH (runQ)
import Data.Monoid (Sum)

class FooClass a b

data MyRecord = MyRecord
  { myA :: Int,
    myB :: Float
  }

data MyRecordGrad = MyRecordGrad
  { myA :: Int,
    myB :: Float
  }

data MyRecordGradBuilder = MyRecordGradBuilder
  { myA :: Sum Int,
    myB :: Sum Float
  }


instD =
  [d|
    instance Semigroup MyRecordGradBuilder where
      MyRecordGradBuilder x1 y1 <> MyRecordGradBuilder x2 y2 = MyRecordGradBuilder (x1<>x2) (y1<>y2)
  |]

data InfixC = Int :^^^ Float

--mkTang ''FooClass
--mkTang ''MyRecord
--mkTang ''InfixC

mkRecordSemigroupInstance ''MyRecordGradBuilder

main :: IO ()
main = do
  z <- runQ instD
  print z
  return ()
