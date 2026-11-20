{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Internal.Test.VSpace (
    ModuleTested,
    VSpaceTested,
    HilbertTested,
    FrobeniusTested,
    testModule,
    testVSpace4,
    testHilbertSpace4,
    testFrobeniusAlgebra4,
)
where

import Data.Constraint (Dict (Dict))
import Data.Reflection (Given (given), give)
import Downhill.Algebra (FrobeniusAlgebra (..))
import Downhill.Internal.Test.Ring (AdditiveTested, RingTested, SubtractiveTested)
import Downhill.Linear.Fun (DualOf)
import Downhill.Orphans ()
import Downhill.VSpace (CScalar, InnerSpace (..), VSpace ((|*|)), conj)
import Hedgehog (Gen, forAll, (===))
import NumHask.Prelude hiding (Group)
import Test.Hspec.Core.Spec (SpecM, it)
import Test.Hspec.Hedgehog (hedgehog)

data ModuleWitness v = ModuleWitness
type ModuleTested v = (RingTested (Scalar v), AdditiveTested v, SubtractiveTested v, Given (ModuleWitness v))

testModule ::
    forall v.
    ( RingTested (Scalar v)
    , AdditiveTested v
    , SubtractiveTested v
    , Given (Gen (Scalar v))
    , Given (Gen v)
    , Show v
    , Show (Scalar v)
    , Eq v
    , Module v
    ) =>
    SpecM () (Dict (ModuleTested v))
testModule = do
    let genA = given :: Gen (Scalar v)
        genV = given :: Gen v
    it "1 *| v = v" $ hedgehog $ do
        v <- forAll genV
        one *| v === v
    it "a *| (b *| v) = (a * b) *| v" $ hedgehog $ do
        a <- forAll genA
        b <- forAll genA
        v <- forAll genV
        a *| (b *| v) === (a * b) *| v
    it "a *| v = v |* a" $ hedgehog $ do
        a <- forAll genA
        v <- forAll genV
        a *| v === v |* a
    it "(a + b) *| v = a *| v + b *| v" $ hedgehog $ do
        a <- forAll genA
        b <- forAll genA
        v <- forAll genV
        (a + b) *| v === a *| v + b *| v
    it "a *| (u + v) = a *| u + a *| v" $ hedgehog $ do
        a <- forAll genA
        u <- forAll genV
        v <- forAll genV
        a *| (u + v) === a *| u + a *| v
    return (give (ModuleWitness @v) Dict)

data VSpaceWitness v = VSpaceWitness
type VSpaceTested v = (ModuleTested v, Given (VSpaceWitness v))

testVSpace4 ::
    forall v a.
    ( a ~ Scalar v
    , ModuleTested v
    , VSpace v
    , Given (Gen v)
    , Given (Gen (DualOf v))
    , Given (Gen a)
    , Show v
    , Show a
    , Show (DualOf v)
    , Eq a
    ) =>
    SpecM () (Dict (VSpaceTested v))
testVSpace4 = do
    let
        genA = given :: Gen a
        genV = given :: Gen v
        genDV = given :: Gen (DualOf v)
    it "(a *| dv) |*| v = a * (dv |*| v)" $ hedgehog $ do
        a <- forAll genA
        dv <- forAll genDV
        v <- forAll genV
        (a *| dv) |*| v === a * (dv |*| v)
    it "dv |*| (a *| v) = a * (dv |*| v)" $ hedgehog $ do
        a <- forAll genA
        dv <- forAll genDV
        v <- forAll genV
        dv |*| (a *| v) === a * (dv |*| v)
    it "(dv1 + dv2) |*| v = dv1 |*| v + dv2 |*| v" $ hedgehog $ do
        dv1 <- forAll genDV
        dv2 <- forAll genDV
        v <- forAll genV
        (dv1 + dv2) |*| v === dv1 |*| v + dv2 |*| v
    it "dv |*| (v1 + v2) = dv |*| v1 + dv |*| v2" $ hedgehog $ do
        dv <- forAll genDV
        v1 <- forAll genV
        v2 <- forAll genV
        dv |*| (v1 + v2) === dv |*| v1 + dv |*| v2
    return (give (VSpaceWitness @v) Dict)

data HilbertWitness v = HilbertWitness
type HilbertTested v = (VSpaceTested v, Given (HilbertWitness v))

testHilbertSpace4 ::
    forall v.
    ( VSpaceTested v
    , InnerSpace v
    , Show (Scalar v)
    , Given (Gen (Scalar v))
    , Given (Gen v)
    , Show v
    , Eq (Scalar v)
    , CScalar (Scalar v)
    , Eq v
    ) =>
    SpecM () (Dict (HilbertTested v))
testHilbertSpace4 = do
    let genA = given :: Gen (Scalar v)
    let genV = given :: Gen v
    it "(a *| u) <.> v === a * (u <.> v)" $ hedgehog $ do
        a <- forAll genA
        u <- forAll genV
        v <- forAll genV
        (a *| u) <.> v === a * (u <.> v)
    it "u <.> (a *| v) === conj a * (u <.> v)" $ hedgehog $ do
        a <- forAll genA
        u <- forAll genV
        v <- forAll genV
        u <.> (a *| v) === conj a * (u <.> v)
    it "(u1 + u2) <.> v === u1 <.> v + u2 <.> v" $ hedgehog $ do
        u1 <- forAll genV
        u2 <- forAll genV
        v <- forAll genV
        (u1 + u2) <.> v === u1 <.> v + u2 <.> v
    it "u <.> (v1 + v2) === u <.> v1 + u <.> v2" $ hedgehog $ do
        u <- forAll genV
        v1 <- forAll genV
        v2 <- forAll genV
        u <.> (v1 + v2) === u <.> v1 + u <.> v2
    it "v1 <.> v2 === conj (v2 <.> v1)" $ hedgehog $ do
        v1 <- forAll genV
        v2 <- forAll genV
        v1 <.> v2 === conj (v2 <.> v1)
    it "v1 <.> v2 === riesz v2 |*| v1" $ hedgehog $ do
        v1 <- forAll genV
        v2 <- forAll genV
        v1 <.> v2 === riesz v2 |*| v1
    it "coriesz (riesz v) === v" $ hedgehog $ do
        v <- forAll genV
        coriesz (riesz v) === v
    return (give (HilbertWitness @v) Dict)

data FrobeniusWitness v = FrobeniusWitness
type FrobeniusTested v = (VSpaceTested v, Given (FrobeniusWitness v))

testFrobeniusAlgebra4 ::
    forall v.
    ( VSpaceTested v
    , Given (Gen (Scalar v))
    , Given (Gen v)
    , Show v
    , Show (Scalar v)
    , Eq v
    , FrobeniusAlgebra v
    , Eq (Scalar v)
    , Eq (DualOf v)
    , Show (DualOf v)
    ) =>
    SpecM () (Dict (FrobeniusTested v))
testFrobeniusAlgebra4 = do
    let genV :: Gen v
        genV = given
        genA :: Gen (Scalar v)
        genA = given

    it "trace (a *| v) = a * trace v" $ hedgehog $ do
        a <- forAll genA
        v <- forAll genV
        trace (a *| v) === a * trace v

    it "trace (v1 + v2) = trace v1 + trace v2" $ hedgehog $ do
        v1 <- forAll genV
        v2 <- forAll genV
        trace (v1 + v2) === trace v1 + trace v2

    it "frobenius x |*| y = trace (x * y)" $ hedgehog $ do
        x <- forAll genV
        y <- forAll genV
        frobenius x |*| y === trace (x * y)
    it "cofrobenius (frobenius x) = x" $ hedgehog $ do
        x <- forAll genV
        cofrobenius (frobenius x) === x
    it "trace (x * y) = trace (y * x)" $ hedgehog $ do
        x <- forAll genV
        y <- forAll genV
        trace (x * y) === trace (y * x)

    return (give (FrobeniusWitness @v) Dict)
