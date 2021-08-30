{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module TH(thTest) where

import Test.Tasty (TestTree, testGroup)
import Downhill.TH ( mkDVarC, RecordNamer(..), DVarOptions(..))
import Downhill.Grad (HasGrad(MScalar))
import TestTHOptions (defaultDVarOptions)

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

newtype MyRecord1 = MyRecord1 Float
data MyRecord2 = MyRecord2 Float


mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad MyRecord1 where
      type MScalar MyRecord1 = Float
    |]

mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad MyRecord2 where
      type MScalar MyRecord2 = Float
    |]

data MyRecord3 = MyRecord3

mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad MyRecord3 where
      type MScalar MyRecord3 = ()
    |]

data MyRecord4 a = MyRecord4 a

mkDVarC
  defaultDVarOptions
  [d|
    instance HasGrad a => HasGrad (MyRecord4 a) where
      type MScalar (MyRecord4 a) = MScalar a
    |]

data MyRecord5 a b = MyRecord5 a b

mkDVarC
  defaultDVarOptions
  [d|
    instance (HasGrad a, HasGrad b, MScalar a ~ MScalar b) => HasGrad (MyRecord5 a b) where
      type MScalar (MyRecord5 a b) = MScalar a
    |]

data MyRecord6 a b = MyRecord6 a b

mkDVarC
  defaultDVarOptions
  [d|
    instance (HasGrad a, MScalar a ~ Float) => HasGrad (MyRecord6 a Float) where
      type MScalar (MyRecord6 a Float) = Float
    |]


thTest :: TestTree
thTest = testGroup "Template Haskell" []
