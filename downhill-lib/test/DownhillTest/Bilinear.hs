{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module DownhillTest.Bilinear where

import Data.AffineSpace ((.+^))
import Data.VectorSpace (AdditiveGroup, VectorSpace ((*^)), (^+^))
import Downhill.BVar (BVar (bvarValue))
import qualified Downhill.BVar as BVar
import Downhill.Grad (Dual (evalGrad), HasFullGrad, HasGrad (Grad, MScalar))
import Hedgehog
  ( Gen,
    Property,
    forAll,
    property,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import GHC.Base (VecElem(DoubleElemRep))
import Hedgehog.Internal.Show (Value(Integer))
import qualified Hedgehog.Internal.Show as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testBilinear ::
  ( Show u,
    Show v,
    HasGrad u,
    HasGrad v,
    Show (Grad z),
    HasFullGrad z,
    Eq z,
    AdditiveGroup u,
    Show z,
    AdditiveGroup z,
    Dual (MScalar z) (Grad u) u,
    Eq (MScalar z),
    Dual (MScalar z) (Grad z) z, Show (MScalar z), Dual (MScalar z) (Grad v) v) =>
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

bilinearTests :: TestTree
bilinearTests =
   testGroup "Bilinear operations"
     [ testProperty "Scalar multiplication"
         (testBilinear ((*) @Integer) (*) genInt genInt genInt)
     ]
  
  where
    scalarMul :: Integer -> Integer -> Integer
    scalarMul = (*)
    genInt :: Gen Integer
    genInt = Gen.integral (Range.linear (-100) 100)
    --scalarMulBVar :: BVar r Integer -> BVar r 