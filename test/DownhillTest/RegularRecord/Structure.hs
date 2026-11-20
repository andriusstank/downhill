{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.RegularRecord.Structure (structureTests) where

import Data.Singletons (Sing, SingI (..), type (@@))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector.Sized qualified as Vector
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Manifold (Manifold (..))
import Downhill.Tree.RTree (LineNode (..), LineTree (..), RegularTree (..), regularTreeLineTree, renderLineTree)
import Downhill.Tree.RegularRecord (FieldNaming (..), IsRegularRecord (..), RegularTreeShape (..))
import Downhill.VSpace (VSpace (..))
import DownhillTest.RegularRecord.Fixtures
import NumHask.Prelude
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)

testGenericRoundTrips :: Assertion
testGenericRoundTrips = do
    fromRegularRecordFields (toRegularRecordFields namedRecord) @=? namedRecord
    fromRegularRecordFields (toRegularRecordFields nestedRecord) @=? nestedRecord
    fromRegularRecordFields (toRegularRecordFields tupleRecord) @=? tupleRecord
    fromRegularRecordFields (toRegularRecordFields nestedNewtype) @=? nestedNewtype
    fromRegularRecordFields (toRegularRecordFields scalarNewtype) @=? scalarNewtype
  where
    namedRecord :: MyRecordA (VarLeaf Int)
    namedRecord = MyRecordA 5 (Vector.fromTuple (7, 11))

    nestedRecord :: MyRecordB (VarLeaf Int)
    nestedRecord = MyRecordB 3 namedRecord

    tupleRecord :: MyTupleRecord (VarLeaf Int)
    tupleRecord = MyTupleRecord 13 (Vector.fromTuple (17, 19))

    nestedNewtype :: MyNestedNewtype (VarLeaf Int)
    nestedNewtype = MyNestedNewtype namedRecord

    scalarNewtype :: MyScalarNewtype (VarLeaf Int)
    scalarNewtype = MyScalarNewtype 23

-- Check the associated defaults, including a positional wrapper whose child
-- is a named record: only the outer constructor determines its naming.
testGenericFieldNaming :: Assertion
testGenericFieldNaming =
    case ( Refl :: FieldNamingOf MyRecordA :~: 'Named
         , Refl :: FieldNamingOf MyTupleRecord :~: 'Unnamed
         , Refl :: FieldNamingOf MyNestedNewtype :~: 'Unnamed
         ) of
        (Refl, Refl, Refl) -> pure ()

testRRecord :: Assertion
testRRecord = fromRegularRecordFields (toRegularRecordFields record) @=? record
  where
    record :: MyRecordB (VarLeaf Int)
    record = MyRecordB 3 (MyRecordA 5 (Vector.fromTuple (7, 11)))

testPrintRegularRecord :: Assertion
testPrintRegularRecord = do
    regularTreeLineTree @(VarLeaf Int) printLeaf sing tree @=? expectedTree
    renderLineTree expectedTree @=? expectedText
  where
    printLeaf :: forall (sh :: LeafShape). Sing sh -> VarLeaf Int @@ sh -> LineTree
    printLeaf SLshScalar = LineInline . Text.pack . show
    printLeaf SLshVector = LineInline . Text.pack . show . toList

    record :: MyRecordB (VarLeaf Int)
    record = MyRecordB 3 (MyRecordA 5 (Vector.fromTuple (7, 11)))
    tree :: RegularTree (VarLeaf Int) ('RegularTreeShapeSubtree MyRecordB)
    tree = RegularTree record

    expectedTree :: LineTree
    expectedTree =
        LineBlock
            [ LineNode "scalarFieldB" (LineInline "3")
            , LineNode
                "subrecordField"
                ( LineBlock
                    [ LineNode "scalarField" (LineInline "5")
                    , LineNode "vectorField" (LineInline "[7,11]")
                    ]
                )
            ]

    expectedText :: Text
    expectedText =
        Text.intercalate
            "\n"
            [ "scalarFieldB: 3"
            , "subrecordField:"
            , "  scalarField: 5"
            , "  vectorField: [7,11]"
            ]

testRecordTreeAdditive :: Assertion
testRecordTreeAdditive = do
    splitRecordTreeValues zero @=? (0, 0, 0, 0, 0)
    splitRecordTreeValues (x + y) @=? (4, 7, 12, 18, 28)
  where
    x = splitRecordTree 1 2 5 7 11
    y = splitRecordTree 3 5 7 11 17

testRecordTreeSubtractive :: Assertion
testRecordTreeSubtractive = do
    splitRecordTreeValues (negate x) @=? (-3, -5, -7, -11, -17)
    splitRecordTreeValues (x - y) @=? (2, 3, 2, 4, 6)
  where
    x = splitRecordTree 3 5 7 11 17
    y = splitRecordTree 1 2 5 7 11

testRecordTreeMultiplicativeAction :: Assertion
testRecordTreeMultiplicativeAction =
    splitRecordTreeValues (x |* 3) @=? (6, 9, 15, 21, 33)
  where
    x = splitRecordTree 2 3 5 7 11

testRecordTreeVSpace :: Assertion
testRecordTreeVSpace =
    dual |*| value @=? 2 * 13 + 3 * 17 + 5 * 19 + 7 * 23 + 11 * 29
  where
    dual = splitRecordTree 2 3 5 7 11
    value = splitRecordTree 13 17 19 23 29

testRecordTreeManifold :: Assertion
testRecordTreeManifold = do
    splitRecordTreeValues (point .+~^ tangent) @=? (15, 20, 24, 30, 40)
    splitRecordTreeValues (point .-~^ tangent) @=? (11, 14, 14, 16, 18)
  where
    point = splitRecordTree 13 17 19 23 29
    tangent = splitRecordTree 2 3 5 7 11

testRecordTreeBuilder :: Assertion
testRecordTreeBuilder = do
    splitRecordTreeValues (sumBuilder (identityBuilder x)) @=? splitRecordTreeValues x
    splitRecordTreeValues (sumBuilder (identityBuilder x <> identityBuilder y))
        @=? splitRecordTreeValues (x + y)
  where
    x = splitRecordTree 1 2 5 7 11
    y = splitRecordTree 3 5 7 11 17

structureTests :: SpecWith ()
structureTests = describe "Structure" $ do
    it "derives field naming from the outer constructor" testGenericFieldNaming
    it "round trips generically derived records" testGenericRoundTrips
    it "round trips nested records" testRRecord
    it "prints nested records as a line tree" testPrintRegularRecord
    it "adds record trees" testRecordTreeAdditive
    it "subtracts record trees" testRecordTreeSubtractive
    it "scales record trees" testRecordTreeMultiplicativeAction
    it "pairs record trees as vector spaces" testRecordTreeVSpace
    it "applies record-tree tangent vectors" testRecordTreeManifold
    it "builds record trees" testRecordTreeBuilder

_debugPrintLeaf :: forall (sh :: LeafShape). Sing sh -> VarLeaf Int @@ sh -> LineTree
_debugPrintLeaf SLshScalar = LineInline . Text.pack . show
_debugPrintLeaf SLshVector = LineInline . Text.pack . show . toList

_debugX :: MyRecordB (VarLeaf Int)
_debugX = MyRecordB 3 (MyRecordA 5 (Vector.fromTuple (7, 11)))

-- >>> mapM_ putStrLn (lines (Text.unpack debugText))
