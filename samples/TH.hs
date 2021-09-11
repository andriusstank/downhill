{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Control.Lens.Tuple (_16')
import Data.AdditiveGroup (AdditiveGroup (..))
import Data.Kind (Constraint, Type)
import Data.Monoid (Sum)
import Data.VectorSpace (VectorSpace (Scalar, (*^)))
import Data.AffineSpace (AffineSpace (..))
import qualified Data.VectorSpace as VectorSpace
import Downhill.DVar (BVar (BVar))
import Downhill.Grad (Dual (..), HasGrad (Grad, MScalar, Metric, Tang), MetricTensor (..))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Downhill.Linear.Lift (lift1_sparse)
import Downhill.TH ( defaultDVarOptions, mkDVarC )
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Language.Haskell.TH (Dec, Exp, Pat (ConP), Q, runQ, stringE)
import qualified Language.Haskell.TH as TH

class FooClass a b

newtype MyNewtype = MyNewtype Float

data MyRecord = MyRecord
  { myA :: Float
  ,  myB :: (Float, Float)
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
--mkDVar defaultDVarOptions ''MyRecord


mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad (MyRecord1 Float) where
      type MScalar (MyRecord1 Float) = MScalar Float
    |]

mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad MyNewtype where
      type MScalar MyNewtype = Float
    |]

iq =
  [d||]

{-
instance HasGrad a_a6HB => HasField "myA" (BVar r (MyRecord1 a_a6HB)) (BVar r a_a4jQ) where
  getField (BVar x_a6IJ x_a6IK)
    = (BVar ((getField @"myA") x_a6IJ)) ((lift1_sparse go_a6IL) x_a6IK)
    where
        go_a6IL ::
          VecBuilder (Grad a_a4jQ) -> Maybe (MyRecord1GradTBuilderT a_a6HB)
        go_a6IL dx_da_a6IM
          = Just ((MyRecord1GradDBuilderD dx_da_a6IM) mempty)
-}

test =
  [d|
    instance s ~ VectorSpace.Scalar (MyRecord1 a) => VectorSpace (MyRecord1 s) where
      x *^ y = undefined
    |]

x = 11

main :: IO ()
main = do
  x <- runQ iq
  print x
  return ()
