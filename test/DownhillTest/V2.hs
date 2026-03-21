{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.V2 (V2 (..), Cov2 (..), M2 (..), testM2)
where

import Downhill.BVar (buncurry)
import Downhill.BVar qualified as BVar
import Downhill.Internal.Test.Linear (testBilinearBFun, testHilbertMap, testHilbertSpace, testLinearMap)
import Downhill.Linear.Builder (Dense (Dense), HasBuilder)
import Downhill.Linear.Fun (DualOf, LinearMap (..), SomeLinearMap (..), (<*|), (|*>))
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.VSpace (CScalar (..), InnerSpace (..), VSpace (..))
import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import NumHask.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Downhill.Internal.Test.Ring (testRing)

data V2 a = V2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (V2 a))

data Cov2 a = Cov2 a a
    deriving (Generic, Eq, Show)
    deriving (Additive, Subtractive, MultiplicativeAction) via (Generically (Cov2 a))

type instance DualOf (V2 a) = Cov2 a
type instance DualOf (Cov2 a) = V2 a

instance (CScalar a) => VSpace (V2 a) where
    Cov2 dx dy |*| V2 x y = dx |*| x + dy |*| y

instance (CScalar a) => InnerSpace (V2 a) where
    riesz (V2 x y) = Cov2 (conj x) (conj y)
    coriesz (Cov2 x y) = V2 (conj x) (conj y)
    V2 x1 y1 <.> V2 x2 y2 = x1 <.> x2 + y1 <.> y2

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

instance (CScalar a) => LinearMap (M2 a) (V2 a) (V2 a) where
    lmapFwd (M2 m00 m01 m10 m11) (V2 x y) = V2 x' y'
      where
        x' = m00 * x + m01 * y
        y' = m10 * x + m11 * y
    lmapBack (M2 m00 m01 m10 m11) (Cov2 x y) = Cov2 x' y'
      where
        x' = m00 * x + m10 * y
        y' = m01 * x + m11 * y

traceM2 :: (Additive a) => M2 a -> a
traceM2 (M2 a00 _ _ a11) = a00 + a11

type instance DualOf (M2 a) = M2 a

instance (CScalar a) => VSpace (M2 a) where
    M2 d00 d01 d10 d11 |*| M2 m00 m01 m10 m11 = d00 * m00 + d10 * m01 + d01 * m10 + d11 * m11

-- M2 d00 d01 d10 d11 |*| M2 m00 m01 m10 m11 = d00 * m00 + d01 * m01 + d10 * m10 + d11 * m11

instance (CScalar a) => InnerSpace (M2 a) where
    riesz (M2 m00 m01 m10 m11) = M2 (conj m00) (conj m10) (conj m01) (conj m11)
    coriesz (M2 m00 m01 m10 m11) = M2 (conj m00) (conj m10) (conj m01) (conj m11)
    M2 m00 m01 m10 m11 <.> M2 d00 d01 d10 d11 = m00 <.> d00 + m01 <.> d01 + m10 <.> d10 + m11 <.> d11

genInt :: Gen Integer
genInt = Gen.integral (Range.linearFrom 0 (-10) 10)

genV2 :: Gen a -> Gen (V2 a)
genV2 g = V2 <$> g <*> g

genCov2 :: Gen a -> Gen (Cov2 a)
genCov2 g = Cov2 <$> g <*> g

genCInt :: Gen (Complex Integer)
genCInt = Complex <$> ((,) <$> genInt <*> genInt)

genM2 :: Gen a -> Gen (M2 a)
genM2 g = M2 <$> g <*> g <*> g <*> g

testMMul :: TestTree
testMMul = testGroup "MMul" [assocRight, assocLeft]
  where
    assocRight = testProperty "" . property $ do
        a <- forAll (genM2 genCInt)
        b <- forAll (genM2 genCInt)
        v <- forAll (genV2 genCInt)
        (a * b) <*| v === a <*| (b <*| v)
    assocLeft = testProperty "" . property $ do
        a <- forAll (genM2 genCInt)
        b <- forAll (genM2 genCInt)
        dv <- forAll (genCov2 genCInt)
        dv |*> (a * b) === (dv |*> a) |*> b

newtype LeftMul a = LeftMul a

instance (Multiplicative a, InnerSpace a, Multiplicative (DualOf a), DualOf a ~ a) => LinearMap (LeftMul a) a a where
    lmapFwd (LeftMul a) x = a * x

    -- lmapBack (LeftMul a) dx = dx * _ a
    lmapBack (LeftMul a) dx = riesz (riesz a * riesz dx)

-- >>> v |*| (f * u) = (riesz f * v) |*| u

-- >>> u = M2 0 0 1 0 :: M2 Int
-- >>> v = M2 0 0 0 1 :: M2 Int
-- >>> a = M2 0 0 1 0 :: M2 Int
-- >>> f = LeftMul a
-- >>> (riesz(riesz a * v) * u)
-- >>> (riesz( v|*> f) * u)
-- >>> (( v|*> f) |*| u)
-- M2 0 0 0 0
-- M2 0 0 0 0
-- 1

-- >>> (v,  (f <*| u))
-- >>> (v,  riesz (riesz u * riesz a))
-- (M2 0 0 0 1,M2 0 0 0 0)
-- (M2 0 0 0 1,M2 0 0 0 0)

testInner :: (Show a, CScalar a, Eq a) => Gen a -> TestTree
testInner gen = testProperty "M2 inner" . property $ do
    a <- forAll (genM2 gen)
    b <- forAll (genM2 gen)
    a |*| b === traceM2 (a * b)

testMBMul :: (Show a, Additive a, CScalar a, Tang a ~ a, Eq a) => Gen a -> TestTree
testMBMul gen = testBilinearBFun gen (genM2 gen) (genM2 gen) (genM2 gen) (*)

-- >>> testInner genCInt
-- No instance for `Show TestTree' arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_ayY6x

testM2 :: TestTree
testM2 =
    testGroup
        "M2"
        [ testHilbertSpace "(V2 Int) Hilbert space" genInt (genV2 genInt) (genCov2 genInt)
        , testHilbertSpace "(V2 (Complex Int)) Hilbert space" genCInt (genV2 genCInt) (genCov2 genCInt)
        , testHilbertSpace "(M2 (Complex Int)) Hilbert space" genCInt (genM2 genCInt) (genM2 genCInt)
        , testRing "Int M2 ring" (genM2 genInt)
        , testRing "Complex M2 ring" (genM2 genCInt)
        , testHilbertMap genCInt (genV2 genCInt) (genCov2 genCInt) (forAll (genM2 genCInt))
        , testLinearMap "Linear" genInt (genV2 genInt) (genCov2 genInt) (SomeLinearMap <$> forAll (genM2 genInt))
        , testHilbertMap genInt (genV2 genInt) (genCov2 genInt) (forAll (genM2 genInt))
        , testLinearMap "Linear" genCInt (genV2 genCInt) (genCov2 genCInt) (SomeLinearMap <$> forAll (genM2 genCInt))
        , testHilbertMap genCInt (genV2 genCInt) (genCov2 genCInt) (forAll (genM2 genCInt))
        , testMMul
        , testInner genCInt
        , testLinearMap "M2 Left mul" genCInt (genM2 genCInt) (genM2 genCInt) (SomeLinearMap . LeftMul <$> forAll (genM2 genCInt))
        , testMBMul genInt
        , testMBMul genCInt
        ]
