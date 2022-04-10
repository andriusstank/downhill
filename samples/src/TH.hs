{- How to use Template Haskell to generate all that boilerplate for records, product types and newtypes -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Uncomment this line to see generated code
--{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main(main) where

import Downhill.Grad (HasGrad (MScalar))
import Downhill.TH (BVarOptions (..), defaultBVarOptions, mkHasGradInstances)

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
