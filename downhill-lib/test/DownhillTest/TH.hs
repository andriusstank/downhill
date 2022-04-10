{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module DownhillTest.TH (thTest) where

import Data.AffineSpace (AffineSpace (..))
import Downhill.Grad (HasGrad (MScalar, Tang))
import Downhill.TH (BVarOptions (..), RecordNamer (..), mkHasGradInstances, TangOptions(..))
import Test.Tasty (TestTree, testGroup)
import DownhillTest.TestTHOptions (defaultDVarOptions)

{-# ANN module "HLint: ignore Use newtype instead of data" #-}
newtype MyRecord1 = MyRecord1 Float

data MyRecord2 = MyRecord2 Float

mkHasGradInstances
  defaultDVarOptions
  [d|
    instance HasGrad MyRecord1 where
      type MScalar MyRecord1 = Float
    |]

mkHasGradInstances
  defaultDVarOptions
  [d|
    instance HasGrad MyRecord2 where
      type MScalar MyRecord2 = Float
    |]

data MyRecord3 = MyRecord3

mkHasGradInstances
  defaultDVarOptions
  [d|
    instance HasGrad MyRecord3 where
      type MScalar MyRecord3 = ()
    |]

data MyRecord4 a = MyRecord4 a

mkHasGradInstances
  defaultDVarOptions
  [d|
    instance (AffineSpace a, HasGrad a, Diff a ~ Tang a) => HasGrad (MyRecord4 a) where
      type MScalar (MyRecord4 a) = MScalar a
    |]

data MyRecord5 a b = MyRecord5 a b

mkHasGradInstances
  defaultDVarOptions
  [d|
    instance
      ( AffineSpace a,
        AffineSpace b,
        HasGrad a,
        HasGrad b,
        MScalar a ~ MScalar b,
        Diff a ~ Tang a,
        Diff b ~ Tang b
      ) =>
      HasGrad (MyRecord5 a b)
      where
      type MScalar (MyRecord5 a b) = MScalar a
    |]

data MyRecord6 a b = MyRecord6 a b

mkHasGradInstances
  defaultDVarOptions
  [d|
    instance
      ( AffineSpace a,
        HasGrad a,
        MScalar a ~ Float,
        Diff a ~ Tang a
      ) =>
      HasGrad (MyRecord6 a Float)
      where
      type MScalar (MyRecord6 a Float) = Float
    |]

data MyRecord7 a = MyRecord7
  { myField7 :: a
  , myLabel7 :: String
  }

mkHasGradInstances
  defaultDVarOptions {optExcludeFields = ["myLabel7"]}
  [d|
    instance HasGrad a => HasGrad (MyRecord7 a) where
      type MScalar (MyRecord7 a) = MScalar a
    |]

data MyRecord8 = MyRecord8 Float

mkHasGradInstances
  defaultDVarOptions { optTang = SameTang }
  [d|
    instance HasGrad MyRecord8 where
      type MScalar MyRecord8 = Float
    |]

thTest :: TestTree
thTest = testGroup "Template Haskell" [] -- just test if it compiles...
