{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DownhillTest.Tree (treeTests)
where

import Data.Singletons (Apply, SingI (sing), TyCon1, type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Tree.FieldWrapper (DenseFieldWrapper (..), FieldWrapper (..))
import Downhill.Tree.Record (
    FieldGetter (FieldGetter),
    IsRecord (..),
    Record,
    RecordBuilder,
    ShR (..),
    fieldBuilder,
    recordField,
 )
import Downhill.Tree.Tree (TreeOf, TreeShape (TreeShapeLeaf, TreeShapeRecord), fillTree)
import DownhillTest.MonoLeaf (Field, MonoLeaf)
import GHC.Records (getField)
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, assert, (@=?))
import Test.Hspec (SpecWith, describe, it)

type Leaf f = f @@ 'TreeShapeLeaf '()
type Subtree f record = f @@ 'TreeShapeRecord record

data MyRecord (f :: TreeShape () ~> Type) = MyRecord
    { fieldA :: Leaf f
    , fieldB :: Leaf f
    }
    deriving (Generic)

data MyBigRecord (f :: TreeShape () ~> Type) = MyBigRecord
    { subrA :: Subtree f MyRecord
    , subrB :: Subtree f MyRecord
    , fieldC :: Leaf f
    }
    deriving (Generic)

instance IsRecord MyRecord
deriving instance (Eq a) => Eq (MyRecord (TreeOf (MonoLeaf a)))
deriving instance (Show a) => Show (MyRecord (TreeOf (MonoLeaf a)))
instance IsRecord MyBigRecord
deriving instance (Eq a) => Eq (MyBigRecord (TreeOf (MonoLeaf a)))
deriving instance (Show a) => Show (MyBigRecord (TreeOf (MonoLeaf a)))
deriving instance Show (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord))
deriving instance Show (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord))

type MyTree = FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord)

testFill1 :: Assertion
testFill1 = myTree @=? expected
  where
    myTree, expected :: MyTree
    myTree = fillTree 4
    expected = FieldWrapper (MyBigRecord (MyRecord 4 4) (MyRecord 4 4) 4)

testEq :: Assertion
testEq = do
    assert (x == x)
    assert (x /= y)
  where
    x, y :: MyTree
    x = FieldWrapper (MyBigRecord (MyRecord 4 4) (MyRecord 4 4) 4)
    y = FieldWrapper (MyBigRecord (MyRecord 4 4) (MyRecord 4 3) 4)

smallBuilderTest :: Assertion
smallBuilderTest = do
    let mkRecord :: Int -> Int -> FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord)
        mkRecord x y = FieldWrapper (MyRecord x y)
        fs :: MyRecord (TyCon1 (FieldGetter MyRecord))
        fs = recordGettersEx
        mkBuilder :: FieldGetter MyRecord ('TreeShapeLeaf '()) -> Int -> VecBuilder (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord))
        mkBuilder field value = FieldWrapper (fieldBuilder field (FieldWrapper (Just (Sum (FieldWrapper value)))))
    sumBuilder mempty @=? mkRecord 0 0
    sumBuilder (identityBuilder (mkRecord 3 4)) @=? mkRecord 3 4
    sumBuilder (mkBuilder fs.fieldA 5) @=? mkRecord 5 0
    sumBuilder (mkBuilder fs.fieldB 6) @=? mkRecord 0 6
    sumBuilder (mkBuilder fs.fieldA 5 <> mkBuilder fs.fieldA 6) @=? mkRecord 11 0
    sumBuilder (identityBuilder (mkRecord 7 8) <> mkBuilder fs.fieldA 5 <> mkBuilder fs.fieldA 6) @=? mkRecord 18 8
    sumBuilder (mkBuilder fs.fieldB 9) @=? mkRecord 0 9
    sumBuilder (mkBuilder fs.fieldB 9 <> mkBuilder fs.fieldA 10) @=? mkRecord 10 9
    sumBuilder (mkBuilder fs.fieldB 9 <> mkBuilder fs.fieldA 10 <> identityBuilder (mkRecord 10 20)) @=? mkRecord 20 29

bigBuilderTest :: Assertion
bigBuilderTest = do
    let mkSmallRecord :: Int -> Int -> FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord)
        mkSmallRecord x y = FieldWrapper (MyRecord x y)
        mkBuilder :: FieldGetter MyBigRecord ('TreeShapeLeaf '()) -> Int -> VecBuilder (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord))
        mkBuilder field value = FieldWrapper (fieldBuilder field (FieldWrapper (Just (Sum (FieldWrapper value)))))
        mkSmallBuilder :: FieldGetter MyRecord ('TreeShapeLeaf '()) -> Int -> VecBuilder (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord))
        mkSmallBuilder field value = FieldWrapper (fieldBuilder field (FieldWrapper (Just (Sum (FieldWrapper value)))))
        wrap :: MyBigRecord (TreeOf (MonoLeaf Int)) -> FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord)
        wrap = FieldWrapper
        fs :: MyBigRecord (TyCon1 (FieldGetter MyBigRecord))
        fs = recordGettersEx
        fsSmall :: MyRecord (TyCon1 (FieldGetter MyRecord))
        fsSmall = recordGettersEx
        mkRBuilder :: FieldGetter MyBigRecord ('TreeShapeRecord MyRecord) -> VecBuilder (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord)) -> VecBuilder (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord))
        mkRBuilder field value = FieldWrapper (fieldBuilder field value)
    sumBuilder mempty @=? wrap (MyBigRecord (MyRecord 0 0) (MyRecord 0 0) 0)
    sumBuilder (mkRBuilder fs.subrA (identityBuilder (mkSmallRecord 31 37))) @=? wrap (MyBigRecord (MyRecord 31 37) (MyRecord 0 0) 0)
    sumBuilder (mkRBuilder fs.subrB (mkSmallBuilder fsSmall.fieldA 41)) @=? wrap (MyBigRecord (MyRecord 0 0) (MyRecord 41 0) 0)
    sumBuilder (mkBuilder fs.fieldC 43) @=? wrap (MyBigRecord (MyRecord 0 0) (MyRecord 0 0) 43)
    return ()


{-
splitMyTree :: BVar r (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord)) ->
    FieldWrapper (Record (MonoLeaf (BVar r a))) ('ShR MyRecord)
splitMyTree = _
-}

treeTests :: SpecWith ()
treeTests = describe "Record" $ do
    it "fill" testFill1
    it "eq" testEq
    it "small builder" smallBuilderTest
    it "big builder" bigBuilderTest
    return ()
