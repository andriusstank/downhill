{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.VSpace.M2 (M2 (..), m2Tests)
where

import Data.Constraint (Dict (Dict))
import Data.Reflection (Given, give)
import Downhill.Algebra (FrobeniusAlgebra (..))
import Downhill.Internal.Test.Ring (RingTested, testAdditive4, testSubtractive4)
import Downhill.Internal.Test.VSpace (testFrobeniusAlgebra4, testHilbertSpace4, testModule, testVSpace4)
import Downhill.Linear.Builder (Dense (..), HasBuilder)
import Downhill.Linear.Fun (DualOf, AsLinearMap (..), LinearMap(..))
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.VSpace (CScalar (..), InnerSpace (..), VSpace (..))
import DownhillTest.VSpace.V2 (Cov2 (..), V2 (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import NumHask.Prelude
import Test.Hspec (Spec, describe, it)
import Downhill.VSpace (RealScalar)
import Test.Hspec.Hedgehog (hedgehog)
import Downhill.Internal.Test.Linear (testBilinearBFun4, testLinearBFun4)

data M2 a = M2 a a a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (M2 a))
    deriving (HasBuilder) via Dense (M2 a)

type instance Tang (M2 a) = M2 a

instance (CScalar a, Tang a ~ a) => Manifold (M2 a) where
    (.+~^) = (+)
    (.-~^) = (+)

instance (Multiplicative a, Additive a) => Multiplicative (M2 a) where
    one = M2 one zero zero one
    M2 a00 a01 a10 a11 * M2 b00 b01 b10 b11 =
        M2
            (a00 * b00 + a01 * b10)
            (a00 * b01 + a01 * b11)
            (a10 * b00 + a11 * b10)
            (a10 * b01 + a11 * b11)

instance (CScalar a) => AsLinearMap (M2 a) (V2 a) (V2 a) where
    toLinearMap (M2 m00 m01 m10 m11) = LinearMap fwd back
      where
        fwd (V2 x y) = V2 x' y'
            where
                x' = m00 * x + m01 * y
                y' = m10 * x + m11 * y
        back (Cov2 x y) = Cov2 x' y'
            where
                x' = m00 * x + m10 * y
                y' = m01 * x + m11 * y

instance (CScalar a) => FrobeniusAlgebra (M2 a) where
    frobenius = id
    cofrobenius = id
    trace (M2 a00 _ _ a11) = a00 + a11

type instance DualOf (M2 a) = M2 a

instance (CScalar a) => VSpace (M2 a) where
    M2 d00 d01 d10 d11 |*| M2 m00 m01 m10 m11 = d00 * m00 + d10 * m01 + d01 * m10 + d11 * m11

instance (CScalar a) => InnerSpace (M2 a) where
    riesz (M2 m00 m01 m10 m11) = M2 (conj m00) (conj m10) (conj m01) (conj m11)
    coriesz (M2 m00 m01 m10 m11) = M2 (conj m00) (conj m10) (conj m01) (conj m11)
    M2 m00 m01 m10 m11 <.> M2 d00 d01 d10 d11 = m00 <.> d00 + m01 <.> d01 + m10 <.> d10 + m11 <.> d11

testM2' ::
    forall a.
    ( Ring a
    , Show a
    , Eq a
    , CScalar a
    , Given (Gen a)
    , Given (Gen (M2 a))
    , RingTested a
    ) =>
    Spec
testM2' = do
    Dict <- testAdditive4 @(M2 a)
    Dict <- testSubtractive4 @(M2 a)
    Dict <- testModule @(M2 a)
    Dict <- testVSpace4 @(M2 a)
    Dict <- testHilbertSpace4 @(M2 a)
    Dict <- testFrobeniusAlgebra4 @(M2 a)
    return ()

genInt :: Gen Integer
genInt = Gen.integral (Range.linearFrom 0 (-10) 10)

genCInt :: Gen (Complex Integer)
genCInt = Complex <$> ((,) <$> genInt <*> genInt)

genM2 :: Gen a -> Gen (M2 a)
genM2 g = M2 <$> g <*> g <*> g <*> g

testM2Int :: (RingTested Integer) => Spec
testM2Int = give genInt $ give (genM2 genInt) (testM2' @Integer)

testM2Complex :: (RingTested (Complex Integer)) => Spec
testM2Complex = give genCInt $ give (genM2 genCInt) (testM2' @(Complex Integer))

testM2Derivatives ::
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
    , Given (Gen a)
    , Show (Scalar a)
    , Show (DualOf a)
    , Given (Gen (Scalar a))
    , Given (Gen (DualOf a))
    , Manifold (Scalar a)
    , HasBuilder (Scalar a)
    , VSpace (DualOf a)
    , CScalar (Scalar a), Manifold (DualOf a), FrobeniusAlgebra a, FrobeniusAlgebra (Scalar a)
    ) =>
    Spec
testM2Derivatives = do
    it "Negate" $ hedgehog $ (testLinearBFun4 @a) negate negate
    it "Scalar vector multiplication" $ hedgehog $ (testBilinearBFun4 @(Scalar a) @a) (*|) (*|)
    it "Dual" $ hedgehog $ (testBilinearBFun4 @(DualOf a) @a @(Scalar a)) (|*|) (|*|)
    it "Multiply" $ hedgehog $ (testBilinearBFun4 @a @a @a) (*) (*)
    it "Trace" $ hedgehog $ (testLinearBFun4 @a @(Scalar a)) trace trace

testM2VectorDerivatives ::
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
    , Given (Gen a)
    , Show (Scalar a)
    , Show (DualOf a)
    , Given (Gen (Scalar a))
    , Given (Gen (DualOf a))
    , Manifold (Scalar a)
    , HasBuilder (Scalar a)
    , RealScalar (Scalar a)
    , InnerSpace a
    , VSpace (DualOf a), FrobeniusAlgebra (Scalar a)
    ) =>
    Spec
testM2VectorDerivatives = do
    it "Inner product" $ hedgehog $ (testBilinearBFun4 @a @a @(Scalar a)) (<.>) (<.>)

m2Tests :: (RingTested Integer, RingTested (Complex Integer)) => Spec
m2Tests = do
    describe "M2 Integer" testM2Int
    describe "M2 (Complex Integer)" testM2Complex
    describe "M2 derivatives" $ do
        give genInt $ give (genM2 genInt) $ give (genM2 genInt) $ testM2Derivatives @(M2 Integer)
        give genCInt $ give (genM2 genCInt) $ give (genM2 genCInt) $ testM2Derivatives @(M2 (Complex Integer))
        give genInt $ give (genM2 genInt) $ give (genM2 genInt) $ testM2VectorDerivatives @(M2 Integer)
