{- How to use Template Haskell to generate all that boilerplate for records, product types and newtypes -}
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

-- Uncomment this line to see generated code
-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Control.Lens.Tuple (_16')
import Data.AdditiveGroup (AdditiveGroup (..))
import Data.AffineSpace (AffineSpace (..))
import Data.Kind (Constraint, Type)
import Data.Monoid (Sum)
import Data.VectorSpace (VectorSpace (Scalar, (*^)))
import qualified Data.VectorSpace as VectorSpace
import Downhill.BVar (BVar (BVar))
import Downhill.Grad (Dual (..), HasGrad (Grad, MScalar, Metric, Tang), MetricTensor (..))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Downhill.Linear.Lift (lift1_sparse)
import Downhill.TH (BVarOptions (..), defaultBVarOptions, mkHasGradInstances)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Language.Haskell.TH (Dec, Exp, Pat (ConP), Q, runQ, stringE)
import qualified Language.Haskell.TH as TH

newtype MyNewtype = MyNewtype Float

data MyRecord = MyRecord
  { myA :: Float,
    myB :: (Float, Float)
  }

data MyRecord2 a = MyRecord1
  { myA :: a,
    myB :: (a, a)
  }

data MyRecord4 a = MyRecord4
  { myField4 :: a,
    myLabel4 :: String
  }

mkHasGradInstances
  defaultBVarOptions
  [d|
    instance HasGrad (MyRecord2 Float) where
      type MScalar (MyRecord2 Float) = MScalar Float
    |]

mkHasGradInstances
  defaultBVarOptions
  [d|
    instance HasGrad MyNewtype where
      type MScalar MyNewtype = Float
    |]

mkHasGradInstances
  defaultBVarOptions {optExcludeFields = ["myLabel4"]}
  [d|
    instance HasGrad a => HasGrad (MyRecord4 a) where
      type MScalar (MyRecord4 a) = MScalar a
    |]

main :: IO ()
main = return ()
