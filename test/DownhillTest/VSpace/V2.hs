{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.VSpace.V2 (V2 (..), Cov2 (..), v2Tests)
where

import Data.Constraint (Dict (Dict))
import Data.Reflection (Given, give)
import Downhill.Algebra (FrobeniusAlgebra)
import Downhill.Internal.Test.Linear (testBilinearBFun4, testLinearBFun4)
import Downhill.Internal.Test.Ring (RingTested, testAdditive4, testSubtractive4)
import Downhill.Internal.Test.VSpace (testHilbertSpace4, testModule, testVSpace4)
import Downhill.Linear.Builder (Dense (..), HasBuilder)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.VSpace (CScalar (..), InnerSpace (..), RealScalar, VSpace (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Show (Value (Integer))
import Hedgehog.Range qualified as Range
import NumHask.Prelude
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

data V2 a = V2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (V2 a))

data Cov2 a = Cov2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (Cov2 a))

type instance Tang (V2 a) = V2 a
type instance Tang (Cov2 a) = Cov2 a

type instance DualOf (V2 a) = Cov2 a
type instance DualOf (Cov2 a) = V2 a

deriving via Dense (V2 a) instance (Additive a) => HasBuilder (V2 a)
deriving via Dense (Cov2 a) instance (Additive a) => HasBuilder (Cov2 a)

instance (Additive a, CScalar a, Tang a ~ a) => Manifold (V2 a) where
    (.+~^) = (+)
    (.-~^) = (-)

instance (Additive a, CScalar a, Tang a ~ a) => Manifold (Cov2 a) where
    (.+~^) = (+)
    (.-~^) = (-)

instance (CScalar a) => VSpace (V2 a) where
    Cov2 dx dy |*| V2 x y = dx |*| x + dy |*| y

instance (CScalar a) => VSpace (Cov2 a) where
    V2 dx dy |*| Cov2 x y = dx |*| x + dy |*| y

instance (CScalar a) => InnerSpace (V2 a) where
    riesz (V2 x y) = Cov2 (conj x) (conj y)
    coriesz (Cov2 x y) = V2 (conj x) (conj y)
    V2 x1 y1 <.> V2 x2 y2 = x1 <.> x2 + y1 <.> y2

instance (CScalar a) => InnerSpace (Cov2 a) where
    riesz (Cov2 x y) = V2 (conj x) (conj y)
    coriesz (V2 x y) = Cov2 (conj x) (conj y)
    Cov2 x1 y1 <.> Cov2 x2 y2 = x1 <.> x2 + y1 <.> y2

testV2' ::
    forall a.
    ( Ring a
    , Show a
    , Eq a
    , CScalar a
    , Given (Gen (V2 a))
    , Given (Gen a)
    , Given (Gen (Cov2 a))
    , RingTested a
    ) =>
    Spec
testV2' = do
    Dict <- testAdditive4 @(V2 a)
    Dict <- testSubtractive4 @(V2 a)
    Dict <- testModule @(V2 a)
    Dict <- testVSpace4 @(V2 a)
    Dict <- testHilbertSpace4 @(V2 a)
    Dict <- testAdditive4 @(Cov2 a)
    Dict <- testSubtractive4 @(Cov2 a)
    Dict <- testModule @(Cov2 a)
    Dict <- testVSpace4 @(Cov2 a)
    Dict <- testHilbertSpace4 @(Cov2 a)
    return ()

genInt :: Gen Integer
genInt = Gen.integral (Range.linearFrom 0 (-10) 10)

genCInt :: Gen (Complex Integer)
genCInt = Complex <$> ((,) <$> genInt <*> genInt)

genV2 :: Gen a -> Gen (V2 a)
genV2 g = V2 <$> g <*> g

genCov2 :: Gen a -> Gen (Cov2 a)
genCov2 g = Cov2 <$> g <*> g

testV2Int :: (RingTested Integer) => Spec
testV2Int = give genInt $ give (genV2 genInt) $ give (genCov2 genInt) (testV2' @Integer)

testV2Complex :: (RingTested (Complex Integer)) => Spec
testV2Complex = give genCInt $ give (genV2 genCInt) $ give (genCov2 genCInt) (testV2' @(Complex Integer))

testVectorDerivatives ::
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
    , CScalar (Scalar a), Manifold (DualOf a), FrobeniusAlgebra (Scalar a)
    ) =>
    Spec
testVectorDerivatives = do
    it "Negate" $ hedgehog $ (testLinearBFun4 @a) negate negate
    it "Scalar vector multiplication" $ hedgehog $ (testBilinearBFun4 @(Scalar a) @a) (*|) (*|)
    it "Dual" $ hedgehog $ (testBilinearBFun4 @(DualOf a) @a @(Scalar a)) (|*|) (|*|)

testRealVectorDerivatives ::
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
testRealVectorDerivatives = do
    it "Inner product" $ hedgehog $ (testBilinearBFun4 @a @a @(Scalar a)) (<.>) (<.>)

v2Tests :: (RingTested Integer, RingTested (Complex Integer)) => Spec
v2Tests = do
    describe "V2 Integer" testV2Int
    describe "V2 (Complex Integer)" testV2Complex
    describe "V2 derivatives" $ do
        give genInt $ give (genV2 genInt) $ give (genCov2 genInt) $ testVectorDerivatives @(V2 Integer)
        give genCInt $ give (genV2 genCInt) $ give (genCov2 genCInt) $ testVectorDerivatives @(V2 (Complex Integer))
        give genInt $ give (genV2 genInt) $ give (genCov2 genInt) $ testRealVectorDerivatives @(V2 Integer)
