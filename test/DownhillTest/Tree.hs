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
import Downhill.BVar (BVar (..), DFun (unDFun))
import Downhill.BVar qualified as BVar
import Downhill.Linear.AD (inlineNode)
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Linear.Fun (DualOf, LinearFun (..))
import Downhill.Manifold (Grad, IsScalar, Manifold, Tang)
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
import Downhill.Tree.Tree (TreeOf, TreeShape (TreeShapeLeaf, TreeShapeRecord), fillTree, joinBVarTree, splitBVarTree)
import DownhillTest.MonoLeaf (Field, MonoLeaf)
import GHC.Records (getField)
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, assert, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Tree.BVar (BVarField(..))

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
        mkBuilder field value = FieldWrapper (fieldBuilder field (FieldWrapper (identityBuilder value)))
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
        mkBuilder field value = FieldWrapper (fieldBuilder field (FieldWrapper (identityBuilder value)))
        mkSmallBuilder :: FieldGetter MyRecord ('TreeShapeLeaf '()) -> Int -> VecBuilder (FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyRecord))
        mkSmallBuilder field value = FieldWrapper (fieldBuilder field (FieldWrapper (identityBuilder value)))
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

splitMyTree ::
    forall r a.
    (Manifold a, HasBuilder (Grad a), HasBuilder (Tang a)) =>
    BVar r (FieldWrapper (TreeOf (MonoLeaf a)) ('TreeShapeRecord MyBigRecord)) ->
    FieldWrapper (TreeOf (MonoLeaf (BVar r a))) ('TreeShapeRecord MyBigRecord)
splitMyTree = splitBVarTree splitField

myFun :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => MyBigRecord (TreeOf (MonoLeaf a)) -> a
myFun x = (x.subrA.fieldA |* 3) + (x.subrA.fieldB |* 5) + (x.subrB.fieldB |* 7) + (x.fieldC |* 11)

myBVarFun ::
    (IsScalar a, HasBuilder a, FromInteger a) =>
    BVar r (FieldWrapper (TreeOf (MonoLeaf a)) ('TreeShapeRecord MyBigRecord)) -> BVar r a
myBVarFun = myFun . unFieldWrapper . splitMyTree

splitBVarTests :: Assertion
splitBVarTests = do
    y @=? 3 * 23 + 5 * 29 + 7 * 37 + 11 * 41
    13 |*> dy @=? FieldWrapper (MyBigRecord (MyRecord (13 * 3) (13 * 5)) (MyRecord 0 (13 * 7)) (13 * 11))
    dy <*| FieldWrapper (MyBigRecord (MyRecord 17 19) (MyRecord 43 47) 53) @=? 3 * 17 + 5 * 19 + 7 * 47 + 11 * 53
  where
    x :: FieldWrapper (TreeOf (MonoLeaf Int)) ('TreeShapeRecord MyBigRecord)
    x = FieldWrapper (MyBigRecord (MyRecord 23 29) (MyRecord 31 37) 41)
    (y, dy) = unDFun (BVar.ad myBVarFun) x

joinMyTree ::
    forall r a.
    (Manifold a, HasBuilder (Grad a), HasBuilder (Tang a)) =>
    FieldWrapper (TreeOf (MonoLeaf (BVar r a))) ('TreeShapeRecord MyBigRecord) ->
    BVar r (FieldWrapper (TreeOf (MonoLeaf a)) ('TreeShapeRecord MyBigRecord))
joinMyTree = joinBVarTree joinField

myFunT :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => a -> MyBigRecord (TreeOf (MonoLeaf a))
myFunT x = MyBigRecord (MyRecord (x |* 3) (x |* 5)) (MyRecord (x |* 7) (x |* 11)) (x |* 17)

myBVarFunT ::
    (IsScalar a, HasBuilder a, FromInteger a) =>
    BVar r a -> BVar r (FieldWrapper (TreeOf (MonoLeaf a)) ('TreeShapeRecord MyBigRecord))
myBVarFunT = joinMyTree . FieldWrapper . myFunT

joinBVarTests :: Assertion
joinBVarTests = do
    y @=? MyBigRecord (MyRecord (13 * 3) (13 * 5)) (MyRecord (13*7) (13 * 11)) (13 * 17)
    FieldWrapper (MyBigRecord (MyRecord 19 23) (MyRecord 29 31) 37) |*> dy @=? 3*19 + 5*23 + 7*29 + 11*31 + 17*37
    dy <*| 41 @=? FieldWrapper (MyBigRecord (MyRecord (41 * 3) (41 * 5)) (MyRecord (41*7) (41 * 11)) (41 * 17))
  where
    x :: Int
    x = 13
    (FieldWrapper y, dy) = unDFun (BVar.ad myBVarFunT) x

treeTests :: SpecWith ()
treeTests = describe "Tree" $ do
    it "fill" testFill1
    it "eq" testEq
    it "small builder" smallBuilderTest
    it "big builder" bigBuilderTest
    it "split" splitBVarTests
    it "join" joinBVarTests
    return ()
