{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.Bilinear where

import Data.AffineSpace ((.+^))
import Data.VectorSpace (AdditiveGroup, VectorSpace (Scalar, (*^)), (^+^))
import Downhill.BVar (BVar (bvarValue))
import qualified Downhill.BVar as BVar
import Downhill.Grad (Dual (evalGrad), HasGrad, MScalar, Manifold (Grad))
import Downhill.Linear.Expr (BasicVector, DenseVector (DenseVector))
import GHC.Base (VecElem (DoubleElemRep))
import GHC.Generics (Generic)
import Hedgehog
  ( Gen,
    Property,
    PropertyT,
    forAll,
    property,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Show (Value (Integer))
import qualified Hedgehog.Internal.Show as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty, testPropertyNamed)

testLinear ::
  forall m r u z.
  ( Show u,
    HasGrad u,
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
    Dual (Grad z) z,
    Show (MScalar z),
    Monad m
  ) =>
  (u -> z) ->
  (forall r. BVar r u -> BVar r z) ->
  Gen u ->
  Gen (Grad z) ->
  PropertyT m ()
testLinear f bf genU genDZ = do
  u <- forAll genU
  dz <- forAll genDZ
  let z = f u
      bu = BVar.var u
      bz = bf bu
      du = BVar.backprop bz dz
  bvarValue bz === z -- check that `f` and `bf` is the same function
  evalGrad u du === evalGrad z dz

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
    Dual (Grad z) z,
    Show (MScalar z),
    Dual (Grad v) v
  ) =>
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

testBilinear' ::
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
    Dual (Grad z) z,
    Show (MScalar z),
    Dual (Grad v) v
  ) =>
  (u -> v -> z) ->
  (forall r. BVar r u -> BVar r v -> BVar r z) ->
  Gen u ->
  Gen v ->
  Gen (Grad z) ->
  Property
testBilinear' f bf genU genV genDZ = property $ do
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

  testLinear (\x -> f x v) (\bx -> bf bx (BVar.constant v)) genU genDZ
  testLinear (\x -> f u x) (\bx -> bf (BVar.constant u) bx) genV genDZ

data Vector = Vector Integer Integer
  deriving (Generic)

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
  testGroup
    "Bilinear operations"
    [ testPropertyNamed "Scalar multiplication" "bilinearIntMulProperty" bilinearIntMulProperty
    -- TODO: scalar-vector product, inner product
    ]
