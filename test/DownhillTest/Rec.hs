{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Rec (recTests)
where

import Data.Monoid qualified as Monoid
import Data.Finite (Finite, finite)
import Data.Singletons (Apply, Sing, SingI (sing), type (@@), type (~>))
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Data.Constraint (Dict (Dict))
import Downhill.Orphans ()
import Downhill.Linear.Builder (HasBuilder (..), vectorElementBuilder)
import Downhill.Tree.At (At (At))
import Downhill.Tree.Rec (HList (..), Rec, RecIndex, fillRec, foldMapRec, foldZipRec, mapRec, recElements, recFieldBuilder, recFromElements, recIndexes, recLookup, traverseRec, traverseRec_, zipRec)
import GHC.TypeLits (KnownNat, Nat)
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (Spec, describe, it)
import Downhill.Tree.Representation (AllConstrained (allConstrained))

data FieldShape = LshScalar | LshVector Nat

data SFieldShape (sh :: FieldShape) where
    SLshScalar :: SFieldShape 'LshScalar
    SLshVector :: (KnownNat n) => SFieldShape ('LshVector n)

type instance Sing = SFieldShape

instance SingI 'LshScalar where
    sing = SLshScalar

instance (KnownNat n) => SingI ('LshVector n) where
    sing = SLshVector

data VarField a :: FieldShape ~> Type

type instance Apply (VarField a) 'LshScalar = a
type instance Apply (VarField a) ('LshVector n) = Vector n a

instance (c a, forall n. (KnownNat n) => c (Vector n a)) => AllConstrained c (VarField a) where
    allConstrained SLshScalar = Dict
    allConstrained SLshVector = Dict

instance (SingI sh, Eq a) => Eq (At (VarField a) sh) where
    At x == At y =
        case sing @sh of
            SLshScalar -> x == y
            SLshVector -> x == y

instance (SingI sh, Show a) => Show (At (VarField a) sh) where
    showsPrec p (At x) =
        case sing @sh of
            SLshScalar -> showsPrec p x
            SLshVector -> showsPrec p x

instance (SingI sh, Additive a) => Additive (At (VarField a) sh) where
    zero =
        case sing @sh of
            SLshScalar -> At zero
            SLshVector -> At zero
    At x + At y =
        case sing @sh of
            SLshScalar -> At (x + y)
            SLshVector -> At (x + y)

instance (SingI sh, Additive a) => HasBuilder (At (VarField a) sh) where
    type VecBuilder (At (VarField a) sh) = Maybe (Sum (At (VarField a) sh))
    sumBuilder = getSum . fromMaybe zero
    identityBuilder = Just . Sum

type TestFields = '[ 'LshScalar, 'LshVector 2, 'LshScalar]

testRecord :: Rec (VarField Int) TestFields
testRecord = recFromElements (sing @TestFields) testElements

testElements :: HList '[Int, Vector 2 Int, Int]
testElements = HCons 3 (HCons (Vector.fromTuple (7, 11)) (HCons 5 HNil))

testFromElements :: Assertion
testFromElements =
    recElements @(VarField Int) (sing @TestFields) testRecord @=? testElements

testFillRec :: Assertion
testFillRec =
    expected @=? fillRec @(VarField Int) (sing @TestFields) fillField
  where
    fillField :: forall sh. Sing sh -> VarField Int @@ sh
    fillField SLshScalar = 3
    fillField SLshVector = Vector.replicate 5

    expected :: Rec (VarField Int) TestFields
    expected = recFromElements (sing @TestFields) (HCons 3 (HCons (Vector.fromTuple (5, 5)) (HCons 3 HNil)))

testMapRec :: Assertion
testMapRec =
    expected @=? mapRec @(VarField Int) @(VarField Int) (sing @TestFields) mapField testRecord
  where
    mapField :: forall sh. Sing sh -> VarField Int @@ sh -> VarField Int @@ sh
    mapField SLshScalar = (* 2)
    mapField SLshVector = Vector.map (+ 1)

    expected :: Rec (VarField Int) TestFields
    expected = recFromElements (sing @TestFields) (HCons 6 (HCons (Vector.fromTuple (8, 12)) (HCons 10 HNil)))

testFoldMapRec :: Assertion
testFoldMapRec =
    foldMapRec (sing @TestFields) foldField testRecord @=? [3, 7, 11, 5]
  where
    foldField :: forall sh. Sing sh -> VarField Int @@ sh -> [Int]
    foldField SLshScalar x = [x]
    foldField SLshVector x = Vector.toList x

testZipRec :: Assertion
testZipRec =
    expected @=? zipRec @(VarField Int) @(VarField Int) @(VarField Int) (sing @TestFields) zipField recordA recordB
  where
    zipField :: forall sh. Sing sh -> VarField Int @@ sh -> VarField Int @@ sh -> VarField Int @@ sh
    zipField SLshScalar = (+)
    zipField SLshVector = Vector.zipWith (+)

    recordA :: Rec (VarField Int) TestFields
    recordA = recFromElements (sing @TestFields) (HCons 3 (HCons (Vector.fromTuple (7, 11)) (HCons 5 HNil)))

    recordB :: Rec (VarField Int) TestFields
    recordB = recFromElements (sing @TestFields) (HCons 13 (HCons (Vector.fromTuple (17, 19)) (HCons 23 HNil)))

    expected :: Rec (VarField Int) TestFields
    expected = recFromElements (sing @TestFields) (HCons 16 (HCons (Vector.fromTuple (24, 30)) (HCons 28 HNil)))

testAdditiveRec :: Assertion
testAdditiveRec = do
    zero @=? zeroRecord
    recordA + recordB @=? expected
  where
    recordA :: Rec (VarField Int) TestFields
    recordA = recFromElements (sing @TestFields) (HCons 3 (HCons (Vector.fromTuple (7, 11)) (HCons 5 HNil)))

    recordB :: Rec (VarField Int) TestFields
    recordB = recFromElements (sing @TestFields) (HCons 13 (HCons (Vector.fromTuple (17, 19)) (HCons 23 HNil)))

    zeroRecord :: Rec (VarField Int) TestFields
    zeroRecord = recFromElements (sing @TestFields) (HCons 0 (HCons (Vector.fromTuple (0, 0)) (HCons 0 HNil)))

    expected :: Rec (VarField Int) TestFields
    expected = recFromElements (sing @TestFields) (HCons 16 (HCons (Vector.fromTuple (24, 30)) (HCons 28 HNil)))

testFoldZipRec :: Assertion
testFoldZipRec =
    Monoid.getSum (foldZipRec (sing @TestFields) zipField recordA recordB) @=? 482
  where
    zipField :: forall sh. Sing sh -> VarField Int @@ sh -> VarField Int @@ sh -> Monoid.Sum Int
    zipField SLshScalar x y = Monoid.Sum (x * y)
    zipField SLshVector x y = Monoid.Sum (sum (Vector.zipWith (*) x y))

    recordA :: Rec (VarField Int) TestFields
    recordA = recFromElements (sing @TestFields) (HCons 3 (HCons (Vector.fromTuple (7, 11)) (HCons 5 HNil)))

    recordB :: Rec (VarField Int) TestFields
    recordB = recFromElements (sing @TestFields) (HCons 13 (HCons (Vector.fromTuple (17, 19)) (HCons 23 HNil)))

testTraverseRec :: Assertion
testTraverseRec = do
    Just expected @=? traverseRec @(VarField Int) @(VarField Int) (sing @TestFields) traverseField validRecord
    Nothing @=? traverseRec @(VarField Int) @(VarField Int) (sing @TestFields) traverseField invalidRecord
  where
    traverseField :: forall sh. Sing sh -> VarField Int @@ sh -> Maybe (VarField Int @@ sh)
    traverseField SLshScalar x
        | x >= 0 = Just (x * 2)
        | otherwise = Nothing
    traverseField SLshVector x = Just (Vector.map (+ 1) x)

    validRecord :: Rec (VarField Int) TestFields
    validRecord = testRecord

    invalidRecord :: Rec (VarField Int) TestFields
    invalidRecord = recFromElements (sing @TestFields) (HCons 3 (HCons (Vector.fromTuple (7, 11)) (HCons (-5) HNil)))

    expected :: Rec (VarField Int) TestFields
    expected = recFromElements (sing @TestFields) (HCons 6 (HCons (Vector.fromTuple (8, 12)) (HCons 10 HNil)))

testTraverseRec_ :: Assertion
testTraverseRec_ =
    getConst (traverseRec_ (sing @TestFields) visitField testRecord) @=? [3, 7, 11, 5]
  where
    visitField :: forall sh. Sing sh -> VarField Int @@ sh -> Const [Int] ()
    visitField SLshScalar x = Const [x]
    visitField SLshVector x = Const (Vector.toList x)

testRecIndexes :: Assertion
testRecIndexes =
    case recElements (sing @TestFields) (recIndexes (sing @TestFields)) of
        HCons scalarIx (HCons vectorIx (HCons scalarIx2 HNil)) -> do
            recLookup scalarIx testRecord @=? 3
            recLookup vectorIx testRecord @=? Vector.fromTuple (7, 11)
            recLookup scalarIx2 testRecord @=? 5

testBuilderRec :: Assertion
testBuilderRec =
    case recElements (sing @TestFields) (recIndexes (sing @TestFields)) of
        HCons scalarIx (HCons vectorIx (HCons scalarIx2 HNil)) -> do
            sumBuilder emptyBuilder @=? zeroRecord
            sumBuilder (identityBuilder recordA) @=? recordA
            sumBuilder (scalarBuilder scalarIx 5) @=? rec 5 (0, 0) 0
            sumBuilder (vectorBuilder vectorIx (17, 19)) @=? rec 0 (17, 19) 0
            sumBuilder (vectorElement vectorIx (finite 0) 17) @=? rec 0 (17, 0) 0
            sumBuilder (vectorElement vectorIx (finite 0) 17 <> vectorElement vectorIx (finite 1) 19) @=? rec 0 (17, 19) 0
            sumBuilder (scalarBuilder scalarIx 5 <> scalarBuilder scalarIx 6) @=? rec 11 (0, 0) 0
            sumBuilder (scalarBuilder scalarIx 5 <> scalarBuilder scalarIx2 6) @=? rec 5 (0, 0) 6
            sumBuilder (identityBuilder recordA <> scalarBuilder scalarIx 5 <> vectorBuilder vectorIx (17, 19)) @=? rec 8 (24, 30) 5
            sumBuilder (identityBuilder recordA <> vectorElement vectorIx (finite 0) 17) @=? rec 3 (24, 11) 5
  where
    emptyBuilder :: VecBuilder (Rec (VarField Int) TestFields)
    emptyBuilder = mempty

    scalarBuilder :: RecIndex TestFields 'LshScalar -> Int -> VecBuilder (Rec (VarField Int) TestFields)
    scalarBuilder index x = recFieldBuilder index (identityBuilder x)

    vectorBuilder :: RecIndex TestFields ('LshVector 2) -> (Int, Int) -> VecBuilder (Rec (VarField Int) TestFields)
    vectorBuilder index x = recFieldBuilder index (identityBuilder (Vector.fromTuple x))

    vectorElement :: RecIndex TestFields ('LshVector 2) -> Finite 2 -> Int -> VecBuilder (Rec (VarField Int) TestFields)
    vectorElement index elementIndex x = recFieldBuilder index (vectorElementBuilder elementIndex (identityBuilder x))

    recordA :: Rec (VarField Int) TestFields
    recordA = rec 3 (7, 11) 5

    zeroRecord :: Rec (VarField Int) TestFields
    zeroRecord = rec 0 (0, 0) 0

    rec :: Int -> (Int, Int) -> Int -> Rec (VarField Int) TestFields
    rec x y z = recFromElements (sing @TestFields) (HCons x (HCons (Vector.fromTuple y) (HCons z HNil)))

testEqRec :: Assertion
testEqRec = do
    recordA @=? recordB
    False @=? (recordA == recordC)
  where
    recordA, recordB, recordC :: Rec (ConstSym1 Int) TestFields
    recordA = recFromElements (sing @TestFields) (HCons 3 (HCons 7 (HCons 5 HNil)))
    recordB = recFromElements (sing @TestFields) (HCons 3 (HCons 7 (HCons 5 HNil)))
    recordC = recFromElements (sing @TestFields) (HCons 3 (HCons 11 (HCons 5 HNil)))

testShowRec :: Assertion
testShowRec =
    "Rec [3,7,5]" @=? show record
  where
    record :: Rec (ConstSym1 Int) TestFields
    record = recFromElements (sing @TestFields) (HCons 3 (HCons 7 (HCons 5 HNil)))

recTests :: Spec
recTests = describe "Rec" $ do
    it "round trips through HList" testFromElements
    it "fills records" testFillRec
    it "maps records" testMapRec
    it "folds records" testFoldMapRec
    it "zips records" testZipRec
    it "adds records" testAdditiveRec
    it "folds zipped records" testFoldZipRec
    it "traverses records" testTraverseRec
    it "traverses records discarding results" testTraverseRec_
    it "constructs indexes for lookup" testRecIndexes
    it "builds records" testBuilderRec
    it "compares records for equality" testEqRec
    it "shows records" testShowRec
