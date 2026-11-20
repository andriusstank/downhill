{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Downhill.Internal.Test.Linear (
    testLinearMap,
    testHilbertMap,
    testBilinearBFun,
    testBilinearity4,
    testLinearMap4,

    testLinearBFun4,
    testBilinearBFun4
) where

import Data.Constraint (Dict (Dict))
import Data.Reflection (Given (given))
import Downhill.BVar (BVar, DFun (DFun), Graph, buncurry)
import Downhill.BVar qualified as BVar
import Downhill.Internal.Test.Ring (checkTested)
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf, AsLinearMap (..), lFst, lSnd, (<*|), (|*>), LinearMap (LinearMap), lFlip)
import Downhill.Manifold (Manifold, Tang)
import Downhill.Orphans ()
import Downhill.VSpace (InnerSpace (..), VSpace ((|*|)))
import Hedgehog (Gen, Group (Group), PropertyT, forAll, property, (===))
import NumHask.Prelude hiding (Group)
import Test.HUnit (Assertion)
import Test.Hspec (Spec, it)
import Test.Hspec.Hedgehog (hedgehog)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

testLinearity ::
    ( a ~ Scalar u
    , a ~ Scalar v
    , Show a
    , Show u
    , Eq v
    , MultiplicativeAction u
    , Additive u
    , Show v
    , MultiplicativeAction v
    , Additive v
    ) =>
    TestName -> Gen a -> Gen u -> PropertyT IO (u -> v) -> TestTree
testLinearity name genA genU genF = testGroup name [homogeneity, additivity]
  where
    homogeneity = testProperty "Homogeneity" . property $ do
        f <- genF
        a <- forAll genA
        u <- forAll genU
        f (a *| u) === a *| f u
    additivity = testProperty "Additivity" . property $ do
        f <- genF
        u <- forAll genU
        v <- forAll genU
        f (u + v) === f u + f v

testBilinearity ::
    forall u v w a.
    ( a ~ Scalar u
    , a ~ Scalar v
    , a ~ Scalar w
    , Show a
    , Show u
    , MultiplicativeAction u
    , Additive u
    , Additive w
    , Show v
    , Show w
    , Eq v
    , Eq w
    , Additive v
    , MultiplicativeAction v
    , MultiplicativeAction w
    ) =>
    TestName ->
    Gen a ->
    Gen u ->
    Gen v ->
    PropertyT IO (u -> v -> w) ->
    TestTree
testBilinearity name genA genU genV genF = testGroup name [leftLinearity, rightLinearity]
  where
    bindLeft :: PropertyT IO (v -> w)
    bindLeft = do
        f <- genF
        u <- forAll genU
        return (u `f`)
    bindRight :: PropertyT IO (u -> w)
    bindRight = do
        f <- genF
        v <- forAll genV
        return (`f` v)
    rightLinearity = testLinearity "Right" genA genV bindLeft
    leftLinearity = testLinearity "Left" genA genU bindRight

testBilinearity3 ::
    forall u v w a.
    ( a ~ Scalar u
    , a ~ Scalar v
    , a ~ Scalar w
    , Given (Gen a)
    , Given (Gen u)
    , Given (Gen v)
    , Show a
    , Show u
    , Show v
    , Show w
    , Eq w
    , Semimodule u
    , Semimodule v
    , Semimodule w
    ) =>
    String ->
    (u -> v -> w) ->
    Assertion
testBilinearity3 fname f = do
    Dict <- checkTested () tests
    return ()
  where
    genA = given :: Gen a
    genU = given :: Gen u
    genV = given :: Gen v
    tests =
        Group
            (fromString ("Bilinearity of " ++ fname))
            [ ("(a *| u) ✕ v === a *| (u ✕ v)", homogeneity1)
            , ("u ✕ (a *| v) === a *| (u ✕ v)", homogeneity2)
            , ("(u1 + u2) ✕ v === u1 ✕ v + u2 ✕ v", additivity1)
            , ("u ✕ (v1 + v2) === u ✕ v1 + u ✕ v2", additivity2)
            ]
    homogeneity1 = property $ do
        a <- forAll genA
        u <- forAll genU
        v <- forAll genV
        f (a *| u) v === a *| f u v
    homogeneity2 = property $ do
        a <- forAll genA
        u <- forAll genU
        v <- forAll genV
        f u (a *| v) === a *| f u v
    additivity1 = property $ do
        u1 <- forAll genU
        u2 <- forAll genU
        v <- forAll genV
        f (u1 + u2) v === f u1 v + f u2 v
    additivity2 = property $ do
        u <- forAll genU
        v1 <- forAll genV
        v2 <- forAll genV
        f u (v1 + v2) === f u v1 + f u v2

testBilinearity4 ::
    forall u v w a.
    ( a ~ Scalar u
    , a ~ Scalar v
    , a ~ Scalar w
    , Given (Gen a)
    , Given (Gen u)
    , Given (Gen v)
    , Show a
    , Show u
    , Show v
    , Show w
    , Eq w
    , Semimodule u
    , Semimodule v
    , Semimodule w
    ) =>
    (u -> v -> w) -> Spec
testBilinearity4 f = do
    let genA = given :: Gen a
        genU = given :: Gen u
        genV = given :: Gen v
    it "(a *| u) ✕ v === a *| (u ✕ v)" $ hedgehog $ do
        a <- forAll genA
        u <- forAll genU
        v <- forAll genV
        f (a *| u) v === a *| f u v
    it "u ✕ (a *| v) === a *| (u ✕ v)" $ hedgehog $ do
        a <- forAll genA
        u <- forAll genU
        v <- forAll genV
        f u (a *| v) === a *| f u v
    it "(u1 + u2) ✕ v === u1 ✕ v + u2 ✕ v" $ hedgehog $ do
        u1 <- forAll genU
        u2 <- forAll genU
        v <- forAll genV
        f (u1 + u2) v === f u1 v + f u2 v
    it "u ✕ (v1 + v2) === u ✕ v1 + u ✕ v2" $ hedgehog $ do
        u <- forAll genU
        v1 <- forAll genV
        v2 <- forAll genV
        f u (v1 + v2) === f u v1 + f u v2

testLinearMap ::
    forall u v.
    ( Scalar u ~ Scalar v
    , Eq (Scalar v)
    , Show (Scalar v)
    , Show u
    , Show (DualOf v)
    , Show (DualOf u)
    , Eq (DualOf u)
    , Eq v
    , Show v
    , VSpace v
    , VSpace u
    ) =>
    TestName -> Gen (Scalar u) -> Gen u -> Gen (DualOf v) -> PropertyT IO (LinearMap u v) -> TestTree
testLinearMap name genA genU genDV genF =
    testGroup
        name
        [ testLinearity "Left" genA genU leftF
        , testLinearity "Right" genA genDV rightF
        , testAdjoint
        ]
  where
    leftF :: PropertyT IO (u -> v)
    leftF = do
        f <- genF
        return (f <*|)
    rightF :: PropertyT IO (DualOf v -> DualOf u)
    rightF = do
        f <- genF
        return (|*> f)
    testAdjoint = testProperty "Adjoint" . property $ do
        f <- genF
        u <- forAll genU
        v <- forAll genDV
        (v |*> f) |*| u === v |*| (f <*| u)

testLinearMap4 ::
    forall f u v.
    ( Scalar u ~ Scalar v
    , Eq (Scalar v)
    , Show (Scalar v)
    , Show u
    , Show (DualOf v)
    , Show (DualOf u)
    , Eq (DualOf u)
    , Eq v
    , Show v
    , VSpace v
    , VSpace u
    , AsLinearMap f u v
    , Given (Gen (Scalar u))
    , Given (Gen u)
    , Given (Gen (DualOf v))
    ) =>
    f -> PropertyT IO ()
testLinearMap4 f = do
    let genA = given :: Gen (Scalar v)
        genU = given :: Gen u
        genDV = given :: Gen (DualOf v)
    do
        u <- forAll genU
        dv <- forAll genDV
        (dv |*> f) |*| u === dv |*| (f <*| u)
    do
        a <- forAll genA
        u <- forAll genU
        f <*| (a *| u) === a *| (f <*| u)
    do
        a <- forAll genA
        dv <- forAll genDV
        (a *| dv) |*> f === a *| (dv |*> f)
    do
        v1 <- forAll genU
        v2 <- forAll genU
        f <*| (v1 + v2) === f <*| v1 + f <*| v2
    do
        dv1 <- forAll genDV
        dv2 <- forAll genDV
        (dv1 + dv2) |*> f === dv1 |*> f + dv2 |*> f
    return ()

testHilbertMap ::
    forall f a u v.
    ( VSpace v
    , VSpace u
    , VSpace f
    , Scalar u ~ a
    , Scalar v ~ a
    , AsLinearMap f u v
    , AsLinearMap (DualOf f) v u
    , Show u
    , Show v
    , Eq v
    , Eq (DualOf v)
    , InnerSpace v
    , Show (DualOf v)
    , InnerSpace u
    , InnerSpace f
    , Show a
    , Show (DualOf u)
    , Eq a
    , Eq (DualOf u)
    , Eq u
    ) =>
    Gen a -> Gen u -> Gen (DualOf v) -> PropertyT IO f -> TestTree
testHilbertMap genA genU genDV genF =
    testGroup
        "Hilbert Linear Map"
        [ testLinearMap "Linear map" genA genU genDV (toLinearMap <$> genF)
        , testAdjoint
        , testAdjoint'
        ]
  where
    testAdjoint = testProperty "Adjoint" . property $ do
        f <- genF
        x <- forAll genU
        riesz (f <*| x) === riesz x |*> riesz f
    testAdjoint' = testProperty "Adjoint'" . property $ do
        f <- genF
        x <- forAll genDV
        coriesz (x |*> f) === riesz f <*| coriesz x

testHilberMul :: (Show a, InnerSpace a, Multiplicative a, Multiplicative (DualOf a), Eq a) => Gen a -> TestTree
testHilberMul gen = testProperty "???" . property $ do
    a <- forAll gen
    b <- forAll gen
    a * b === coriesz (riesz b * riesz a)

testLinearDFun ::
    forall u v.
    ( Eq v
    , Eq (Scalar u)
    , Eq (DualOf u)
    , VSpace u
    , VSpace v
    , Scalar u ~ Scalar v
    , Show v
    , Show u
    , Show (Scalar u)
    , Show (DualOf v)
    , Show (DualOf u)
    , u ~ Tang u
    , v ~ Tang v
    ) =>
    Gen (Scalar v) ->
    Gen u ->
    Gen (DualOf v) ->
    PropertyT IO (DFun u v) ->
    TestTree
testLinearDFun genScalar genU genDV genF = testGroup "Linear DFun" [selfD, linearity]
  where
    selfD = testProperty "f = Df" . property $ do
        DFun f <- genF
        x1 <- forAll genU
        x2 <- forAll genU
        let (fx, _df) = f x1
            (_fx, df) = f x2
        fx === df <*| x1
    derivative :: PropertyT IO (LinearMap u v)
    derivative = do
        DFun f <- genF
        x <- forAll genU
        return (snd (f x))
    -- linearity = _ (runSomeLinearMap (testLinearMap "Linearity of derivative" genScalar genU genDV) <$> derivative)
    linearity = testLinearMap "Linearity of derivative" genScalar genU genDV derivative

testLinearBFun ::
    forall u v.
    ( Eq v
    , VSpace u
    , VSpace v
    , VSpace (DualOf u)
    , VSpace (DualOf v)
    , HasBuilder u
    , HasBuilder v
    , HasBuilder (DualOf u)
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar u)
    , Scalar u ~ Scalar v
    , Tang v ~ v
    , Tang u ~ u
    , Tang (Scalar v) ~ Scalar v
    , Manifold (Scalar v)
    , Eq (DualOf u)
    , Eq (Scalar v)
    , Show (Scalar v)
    , Show u
    , Show (DualOf v)
    , Show v
    , Show (DualOf u)
    ) =>
    Gen (Scalar v) -> Gen u -> Gen (DualOf v) -> (forall r. BVar r u -> BVar r v) -> TestTree
testLinearBFun genScalar genU genDV bf = testLinearDFun genScalar genU genDV (pure (BVar.ad bf))

testLinearBFun4 ::
    forall u v.
    ( Given (Gen u)
    , Show u
    , Eq v
    , Show v
    , HasBuilder u
    , HasBuilder (DualOf u)
    , HasBuilder v
    , HasBuilder (DualOf v)
    , Manifold u
    , Manifold v
    , Scalar v ~ Scalar u
    , Eq v
    , Eq (Scalar u)
    , Eq (DualOf u)
    , Show (Scalar u)
    , Show v
    , Show u
    , Show (DualOf v)
    , Show (DualOf u)
    , Given (Gen (Scalar u))
    , Given (Gen u)
    , Given (Gen (DualOf v))
    , Tang v ~ v
    , Tang u ~ u
    ) =>
    (u -> v) -> (forall r. BVar r u -> BVar r v) -> PropertyT IO ()
testLinearBFun4 f bf = do
    df <- do
        let (yz, dyz) = BVar.backprop . bf $ BVar.var zero
        f zero === zero
        yz === zero
        return (dyz :: Graph u v)
    let genU :: Gen u
        genU = given
        genDV :: Gen (DualOf v)
        genDV = given
    x0 <- forAll genU
    let (y0, df0) = BVar.backprop . bf $ BVar.var x0
    x <- forAll genU
    dx <- forAll genDV

    df <*| x === df0 <*| x
    dx |*> df === dx |*> df0
    y0 === df0 <*| x0

    testLinearMap4 df
    return ()

testBilinearBFun4 ::
    forall u v w.
    ( Scalar u ~ Scalar v
    , Scalar u ~ Scalar w
    , Manifold u
    , Manifold v
    , Manifold w
    , Tang u ~ u
    , Tang v ~ v
    , Tang w ~ w
    , Given (Gen u)
    , Given (Gen v)
    , Show u
    , Given (Gen (Scalar w))
    , Given (Gen (DualOf w))
    , HasBuilder v
    , HasBuilder w
    , HasBuilder (DualOf v)
    , HasBuilder (DualOf w)
    , Eq w
    , Eq (Scalar w)
    , Eq (DualOf v)
    , Show w
    , Show v
    , Show (Scalar w)
    , Show (DualOf w)
    , Show (DualOf v)
    , HasBuilder u
    , HasBuilder (DualOf u)
    , Eq (DualOf u)
    , Show (DualOf u)
    ) =>
    (u -> v -> w) -> (forall r. BVar r u -> BVar r v -> BVar r w) -> PropertyT IO ()
testBilinearBFun4 f bf = do
    let genU :: Gen u
        genU = given
        genV :: Gen v
        genV = given
    do
        u <- forAll genU
        testLinearBFun4 (f u) (bf (BVar.constant u))
    do
        v <- forAll genV
        testLinearBFun4 (`f` v) (`bf` BVar.constant v)

data BoundLeftLinearMap u v w f = BoundLeftLinearMap u f

instance (AsLinearMap f (u, v) w, Scalar v ~ Scalar w) => AsLinearMap (BoundLeftLinearMap u v w f) v w where
    toLinearMap (BoundLeftLinearMap u f) = LinearMap (fwd . (u,)) (snd . back)
        where
            LinearMap fwd back = toLinearMap f


constFirst ::
    forall a b.
    (DualOf (DualOf (Tang a)) ~ Tang a, DualOf (DualOf (Tang b)) ~ Tang b, Additive (Tang a)) =>
    a -> DFun b (a, b)
constFirst x = DFun go
  where
    go :: b -> ((a, b), LinearMap (Tang b) (Tang a, Tang b))
    go y = ((x, y), lFlip lSnd)

constSecond ::
    forall a b.
    (DualOf (DualOf (Tang a)) ~ Tang a, DualOf (DualOf (Tang b)) ~ Tang b, Additive (Tang b)) =>
    b -> DFun a (a, b)
constSecond x = DFun go
  where
    go :: a -> ((a, b), LinearMap (Tang a) (Tang a, Tang b))
    go y = ((y, x), lFlip lFst)

testBilinearDFun ::
    forall u v w.
    ( Eq v
    , Eq (Scalar u)
    , Eq (DualOf u)
    , VSpace u
    , VSpace v
    , VSpace w
    , Show v
    , Show u
    , Show (Scalar u)
    , Show (DualOf v)
    , Show (DualOf u)
    , Tang v ~ v
    , Tang w ~ w
    , Tang u ~ u
    , Scalar v ~ Scalar w
    , Eq w
    , Eq (Scalar w)
    , Eq (DualOf v)
    , Show w
    , Show (Scalar w)
    , Show (DualOf w)
    , Scalar u ~ Scalar w
    ) =>
    Gen (Scalar v) ->
    Gen u ->
    Gen v ->
    Gen (DualOf w) ->
    DFun (u, v) w ->
    TestTree
testBilinearDFun genScalar genU genV genDW f = testGroup "Bilinear DFun" [testLinearDFun genScalar genV genDW bindLeft, testLinearDFun genScalar genU genDW bindRight]
  where
    bindLeft :: PropertyT IO (DFun v w)
    bindLeft = do
        u <- forAll genU
        return (f . constFirst u)
    bindRight :: PropertyT IO (DFun u w)
    bindRight = do
        v <- forAll genV
        return (f . constSecond v)

testBilinearBFun ::
    forall u v w.
    ( VSpace u
    , VSpace v
    , VSpace w
    , Tang v ~ v
    , Tang w ~ w
    , Tang u ~ u
    , Scalar v ~ Scalar w
    , Scalar u ~ Scalar w
    , Eq v
    , Eq w
    , Eq (Scalar w)
    , Eq (DualOf u)
    , Eq (DualOf v)
    , Show v
    , Show u
    , Show w
    , Show (Scalar w)
    , Show (DualOf v)
    , Show (DualOf u)
    , Show (DualOf w)
    , HasBuilder u
    , HasBuilder v
    , HasBuilder (DualOf u)
    , HasBuilder (DualOf v)
    , HasBuilder w
    , HasBuilder (DualOf w)
    ) =>
    Gen (Scalar v) ->
    Gen u ->
    Gen v ->
    Gen (DualOf w) ->
    (forall r. BVar r u -> BVar r v -> BVar r w) ->
    TestTree
testBilinearBFun genScalar genU genV genDW f = testBilinearDFun genScalar genU genV genDW (BVar.ad (buncurry f))

{-

testBilinearBFun ::
    ( VSpace u1
    , VSpace u2
    , VSpace v
    , Tang u1 ~ u1
    , Tang v ~ v
    , Tang (Scalar v) ~ Scalar v
    , Scalar u1 ~ Scalar v
    , Scalar u2 ~ Scalar v
    , HasBuilder u1
    , HasBuilder v
    , HasBuilder (DualOf u1)
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    , Manifold (Scalar v)
    , VSpace (DualOf u1)
    , VSpace (DualOf v)
    , Eq v
    , Eq (DualOf u1)
    , Eq (Scalar v)
    , Show v
    , Show (Scalar v)
    , Show (DualOf v)
    , Show (DualOf u1)
    , Show u2
    , Show u1
    , Tang u2 ~ u2
    , Tang (Scalar u2) ~ Scalar u2
    , HasBuilder u2
    , HasBuilder (DualOf u2)
    , HasBuilder (Scalar u2)
    , Manifold (Scalar u2)
    , VSpace (DualOf u2)
    , Eq (DualOf u2)
    , Eq (Scalar u2)
    , Show (Scalar u2)
    , Show (DualOf u2)
    ) =>
    Gen (Scalar v) -> Gen u1 -> Gen u2 -> Gen (DualOf v) -> (forall r. BVar r u1 -> BVar r u2 -> BVar r v) -> PropertyT IO ()
testBilinearBFun genScalar genU1 genU2 genDV bf = do
    u1 <- forAll genU1
    u2 <- forAll genU2
    testLinearBFun genScalar genU2 genDV (bf (BVar.constant u1))
    testLinearBFun genScalar genU1 genDV (`bf` BVar.constant u2)
-}
