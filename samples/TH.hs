{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
--{-# OPTIONS_GHC -ddump-to-file #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Control.Lens.Tuple (_16')
import Data.AdditiveGroup (AdditiveGroup (..))
import Data.Kind (Constraint, Type)
import Data.Monoid (Sum)
import Data.VectorSpace (VectorSpace (Scalar, (*^)))
import Downhill.Grad (HasGrad (Grad, MScalar, Tang, Metric), Dual(..), MetricTensor(..))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Downhill.TH
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Exp, Q, runQ, stringE, Pat (ConP))
import qualified Language.Haskell.TH as TH
import qualified Data.VectorSpace as VectorSpace

class FooClass a b

data MyRecord = MyRecord
  { myA :: Float,
    myB :: (Float, Float)
  }

data MyRecord1 a = MyRecord1
  { myA :: a,
    myB :: (a, a)
  }

data MyRecordGradBuilder3 = MyRecordGradBuilder3
  { myA :: Sum Int,
    myB :: Sum Float,
    myC :: Sum Float
  }

data InfixC = Int :^^^ Float

type NoC = () :: Constraint

--mkTang ''FooClass
--mkDVar defaultDVarOptions ''MyRecord
--type Ctx a = Semigroup (VecBuilder (Tang a))
type Ctx a = (BasicVector (Tang a), BasicVector (Grad a))

--mkDVar defaultDVarOptions ''MyRecord1
--mkDVarC defaultDVarOptions ''HasGrad ''MyRecord1

class ScalarB a ~ () => A a

class A a => B a where
  type ScalarB a :: Type

instance A MyRecord

instance B MyRecord where
  type ScalarB MyRecord = ()

--deriving instance Generic MyRecordGradT
--deriving anyclass instance AdditiveGroup MyRecordGradT
--deriving instance Generic MyRecordTangT
--deriving anyclass instance AdditiveGroup MyRecordTangT

--mkTang ''InfixC

--mkRecordSemigroupInstance ''MyRecordGradBuilder
--mkRecordSemigroupInstance ''MyRecordGradBuilder3
--instance AdditiveGroup MyRecordGrad
--instance VectorSpace MyRecordGrad

--mkBasicVectorInstance ''MyRecordGrad ''MyRecordGradBuilder
iq =
  [d|
    instance HasGrad a => HasGrad (MyRecord1 a) where
      type MScalar (MyRecord1 a) = MScalar a
    |]

--mkDVar defaultDVarOptions ''MyRecord

mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad a => HasGrad (MyRecord1 a) where
      type MScalar (MyRecord1 a) = MScalar a
    |]

test = [d| instance s ~ VectorSpace.Scalar (MyRecord1 a) => VectorSpace (MyRecord1 s) where
             x *^ y = undefined
          |]

main :: IO ()
main = do
  x <- runQ test
  print x
  return ()
