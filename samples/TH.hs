{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
--{-# OPTIONS_GHC -ddump-to-file #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Data.Monoid (Sum)
import Data.VectorSpace (AdditiveGroup (zeroV), VectorSpace)
import Downhill.Grad (HasGrad (Grad, Tang))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Downhill.TH
import GHC.Generics (Generic)
import Language.Haskell.TH (runQ, Q, stringE, Exp)
import Control.Lens.Tuple (_16')

class FooClass a b

data MyRecord = MyRecord
  { myA :: Float,
    myB :: (Float, Float)
  }

data MyRecordGradBuilder3 = MyRecordGradBuilder3
  { myA :: Sum Int,
    myB :: Sum Float,
    myC :: Sum Float
  }

data InfixC = Int :^^^ Float

--mkTang ''FooClass
mkDVar defaultDVarOptions ''MyRecord

deriving instance Generic MyRecordGradT
deriving anyclass instance AdditiveGroup MyRecordGradT
deriving instance Generic MyRecordTangT
deriving anyclass instance AdditiveGroup MyRecordTangT

--mkTang ''InfixC

--mkRecordSemigroupInstance ''MyRecordGradBuilder
--mkRecordSemigroupInstance ''MyRecordGradBuilder3
--instance AdditiveGroup MyRecordGrad
--instance VectorSpace MyRecordGrad

--mkBasicVectorInstance ''MyRecordGrad ''MyRecordGradBuilder

main :: IO ()
main = do
  return ()
