{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module DownhillTest.Bilinear where

import Data.AffineSpace ((.+^))
import Data.VectorSpace (AdditiveGroup, VectorSpace ((*^), Scalar), (^+^))
import Downhill.BVar (BVar (bvarValue))
import qualified Downhill.BVar as BVar
import Downhill.Grad (Dual (evalGrad), HasGrad (Grad), MScalar)
import Hedgehog
  ( Gen,
    Property,
    forAll,
    property,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty, testPropertyNamed)
import GHC.Base (VecElem(DoubleElemRep))
import Hedgehog.Internal.Show (Value(Integer))
import qualified Hedgehog.Internal.Show as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import GHC.Generics (Generic)
import Downhill.Linear.Expr (BasicVector, DenseVector (DenseVector))

testBilinear ::
  ( Show u,
    Show v,
    HasGrad u,
    HasGrad v,
    Show (Grad z),
    HasGrad z,
    Eq z,
    AdditiveGroup u,
    Show z,
    AdditiveGroup z,
    Dual (Grad u) u,
    Eq (Scalar u),
    Show (Scalar u),
    Scalar u ~ Scalar z,
    Scalar v ~ Scalar z,
    Dual (Grad z) z, Show (MScalar z), Dual (Grad v) v) =>
  (u -> v -> z) ->
  (forall r. BVar r u -> BVar r v -> BVar r z) ->
  Gen u ->
  Gen v ->
  Gen (Grad z) ->
  Property
testBilinear f bf genU genV genDZ = property $ do
  u <- forAll genU
  v <- forAll genV
  dz <- forAll genDZ
  let z = f u v
      BVar.T2 bu bv = BVar.var (u, v)
      bz = bf bu bv
      (du, dv) = BVar.backprop bz dz
  bvarValue bz === z -- check that `f` and `bf` is the same function
  evalGrad u du === evalGrad z dz
  evalGrad v dv === evalGrad z dz

data Vector = Vector Integer Integer
  deriving Generic

instance AdditiveGroup Vector
instance VectorSpace Vector

bilinearIntMulProperty :: Property
bilinearIntMulProperty = testBilinear ((*) @Integer) (*) genInt genInt genInt
  where
    scalarMul :: Integer -> Integer -> Integer
    scalarMul = (*)
    genInt :: Gen Integer
    genInt = Gen.integral (Range.linear (-100) 100)


bilinearTests :: TestTree
bilinearTests =
   testGroup "Bilinear operations"
     [ testPropertyNamed "Scalar multiplication" "bilinearIntMulProperty" bilinearIntMulProperty
       -- TODO: scalar-vector product, inner product
     ]
