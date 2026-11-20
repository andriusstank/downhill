{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.VSpace.Scalar (funkyDescribe, testScalar4)
where

import Data.Constraint (Dict (Dict))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Reflection (Given, give)
import Downhill.Algebra (FrobeniusAlgebra (..))
import Downhill.Internal.Test.Linear (testLinearBFun4, testBilinearBFun4)
import Downhill.Internal.Test.Ring (RingTested, testAdditive4, testDistributive4, testSubtractive4)
import Downhill.Internal.Test.VSpace (FrobeniusTested, HilbertTested, VSpaceTested, testFrobeniusAlgebra4, testHilbertSpace4, testModule, testVSpace4)
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold, Tang)
import Downhill.VSpace (CScalar (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import NumHask.Prelude
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Hedgehog (hedgehog)
import Downhill.VSpace (InnerSpace((<.>)))
import Downhill.VSpace (RealScalar)
import Downhill.VSpace (VSpace(..))

genInt :: Gen Integer
genInt = Gen.integral (Range.linearFrom 0 (-10) 10)

genCInt :: Gen (Complex Integer)
genCInt = Complex <$> ((,) <$> genInt <*> genInt)

funkyDescribe :: forall r a. String -> SpecM r a -> SpecM r a
funkyDescribe description spec = do
    ref <- runIO (newIORef Nothing)
    describe description $ do
        x <- spec
        runIO (writeIORef ref (Just x))
    res <- runIO $ readIORef ref
    return (fromJust res)

type ScalarTested a = (RingTested a, VSpaceTested a, FrobeniusTested a, HilbertTested a)

testScalarNew ::
    forall a.
    ( Show a
    , Ring a
    , Eq a
    , Given (Gen a)
    , FrobeniusAlgebra a
    , CScalar a
    ) =>
    SpecM () (Dict (ScalarTested a))
testScalarNew = do
    Dict <- testAdditive4 @a
    Dict <- testSubtractive4 @a
    Dict <- testDistributive4 @a
    Dict <- testModule @a
    Dict <- testVSpace4 @a
    Dict <- testHilbertSpace4 @a
    Dict <- testFrobeniusAlgebra4 @a
    return Dict

testScalarDerivatives ::
    forall a.
    ( Subtractive a
    , Manifold a
    , Tang a ~ a
    , HasBuilder a
    , HasBuilder (DualOf a)
    , Eq a
    , Eq (Scalar a)
    , Eq (DualOf a)
    , Show a
    , CScalar a, Given (Gen a), FrobeniusAlgebra a
    ) =>
    Spec
testScalarDerivatives = do
    it "Negate" $ hedgehog $ (testLinearBFun4 @a @a) negate negate
    it "Multiply" $ hedgehog $ (testBilinearBFun4 @a @a @a) (*) (*)
    it "Dual" $ hedgehog $ (testBilinearBFun4 @a @a @a) (|*|) (|*|)
    it "Trace" $ hedgehog $ (testLinearBFun4 @a @a) trace trace

testRealDerivatives :: forall a.
    ( Subtractive a
    , Manifold a
    , Tang a ~ a
    , HasBuilder a
    , HasBuilder (DualOf a)
    , Eq a
    , Eq (Scalar a)
    , Eq (DualOf a)
    , Show a
    , RealScalar a, Given (Gen a), FrobeniusAlgebra a
    ) =>
    Spec
testRealDerivatives = do
    it "Inner product" $ hedgehog $ (testBilinearBFun4 @a @a @a) (<.>) (<.>)

testScalar4 :: SpecM () (Dict (ScalarTested Integer, ScalarTested (Complex Integer)))
testScalar4 = do
    Dict <- funkyDescribe "Integer" $ give genInt (testScalarNew @Integer)
    Dict <- funkyDescribe "Complex Integer" $ give genCInt (testScalarNew @(Complex Integer))

    describe "Derivatives of Integer" $ do
        give genInt (testScalarDerivatives @Integer)
        give genInt (testRealDerivatives  @Integer)
    describe "Derivatives of Complex Integer" $ give genCInt (testScalarDerivatives @(Complex Integer))
    return Dict
