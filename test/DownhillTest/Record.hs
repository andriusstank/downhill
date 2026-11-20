{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Record (recordTests)
where

import Data.Singletons (TyCon1, type (@@), type (~>), SingI (sing))
import Data.Text qualified as Text
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.BVar (BVar (..), unDFun)
import Downhill.BVar qualified as BVar
import Downhill.Algebra (FrobeniusAlgebra)
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Manifold (IsScalar)
import Downhill.Tree.Record (
  FieldGetter,
  IsRecord (..),
  NamedFields (..),
  Record,
  RecordValue (..),
  RecordValueBuilder,
  ShR (..),
  joinBVarRecord,
  recordField,
  recordValueFieldBuilder,
  splitBVarRecord,
  toListRecordEx,
 )
import Downhill.Tree.Representation (HasGradRepr)
import DownhillTest.MonoLeaf (Field, MonoLeaf)
import GHC.Records (getField)
import NumHask.Prelude hiding (Field)
import Prelude.Singletons (ConstSym1)
import Test.HUnit (Assertion, assertFailure, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Linear.Fun ((<*|), (|*>))

data MyRecord (f :: () ~> Type) = MyRecord
  { fieldA :: Field f
  , fieldB :: Field f
  }
  deriving (Generic)

instance IsRecord MyRecord

data MyTupleRecord (f :: () ~> Type) = MyTupleRecord (Field f) (Field f)
  deriving (Generic)

instance IsRecord MyTupleRecord

deriving instance (Eq a) => Eq (MyRecord (MonoLeaf a))
deriving instance (Eq a) => Eq (MyRecord (ConstSym1 a))
deriving instance (Show a) => Show (MyRecord (MonoLeaf a))
deriving instance (Show a) => Show (MyRecord (ConstSym1 a))

testFill1 :: Assertion
testFill1 = fillRecordEx f @=? (MyRecord 3 3 :: MyRecord (ConstSym1 Int))
 where
  f :: STuple0 sh -> ConstSym1 Int @@ sh
  f STuple0 = 3

testFill2 :: Assertion
testFill2 = fillRecordEx f @=? (MyRecord 1 1 :: MyRecord (ConstSym1 Int))
 where
  f :: STuple0 sh -> ConstSym1 Int @@ sh
  f STuple0 = one

testFieldNames :: Assertion
testFieldNames = do
  case fieldNames @MyRecord of
    Just (NamedFields (MyRecord nameA nameB)) ->
      ["fieldA", "fieldB"] @=? fmap Text.unpack [nameA, nameB]
    Nothing -> assertFailure "generic record field names are missing"

  case fieldNames @MyTupleRecord of
    Nothing -> pure ()
    Just _ -> assertFailure "positional constructor unexpectedly has field names"

testPlus :: Assertion
testPlus = RecordValue (MyRecord 8 10) @=? (x + y)
 where
  x, y :: RecordValue MyRecord (MonoLeaf Int)
  x = RecordValue (MyRecord 3 4)
  y = RecordValue (MyRecord 5 6)

testField :: Assertion
testField =
  do
    recordField getters.fieldA x @=? 3
    recordField getters.fieldB x @=? 4
 where
  x :: MyRecord (MonoLeaf Int)
  x = MyRecord 3 4

  getters :: MyRecord (TyCon1 (FieldGetter MyRecord))
  getters = recordGettersEx

builderTests :: SpecWith ()
builderTests = it "builderTests" $ do
  let mkRecord :: Int -> Int -> RecordValue MyRecord (MonoLeaf Int)
      mkRecord x y = RecordValue (MyRecord x y)
      fs :: MyRecord (TyCon1 (FieldGetter MyRecord))
      fs = recordGettersEx
      mkBuilder :: FieldGetter MyRecord '() -> Int -> RecordValueBuilder MyRecord (MonoLeaf Int)
      mkBuilder field value = recordValueFieldBuilder field (identityBuilder value)
  sumBuilder mempty @=? mkRecord 0 0
  sumBuilder (identityBuilder (mkRecord 3 4)) @=? mkRecord 3 4
  sumBuilder (mkBuilder fs.fieldA 5) @=? mkRecord 5 0
  sumBuilder (mkBuilder fs.fieldA 5 <> mkBuilder fs.fieldA 6) @=? mkRecord 11 0
  sumBuilder (identityBuilder (mkRecord 7 8) <> mkBuilder fs.fieldA 5 <> mkBuilder fs.fieldA 6) @=? mkRecord 18 8
  sumBuilder (mkBuilder fs.fieldB 9) @=? mkRecord 0 9
  sumBuilder (mkBuilder fs.fieldB 9 <> mkBuilder fs.fieldA 10) @=? mkRecord 10 9
  sumBuilder (mkBuilder fs.fieldB 9 <> mkBuilder fs.fieldA 10 <> identityBuilder (mkRecord 10 20)) @=? mkRecord 20 29

splitMyRecord ::
  forall r a.
  (HasGradRepr (MonoLeaf a)) =>
  BVar r (RecordValue MyRecord (MonoLeaf a)) ->
  MyRecord (MonoLeaf (BVar r a))
splitMyRecord = splitBVarRecord

myFun :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => MyRecord (MonoLeaf a) -> a
myFun x = (x.fieldA |* 3) + (x.fieldB |* 5)

myBVarFun ::
  (IsScalar a, HasBuilder a, FrobeniusAlgebra a, HasGradRepr (MonoLeaf a)) =>
  BVar r (RecordValue MyRecord (MonoLeaf a)) -> BVar r a
myBVarFun = myFun . splitMyRecord

splitBVarTests :: Assertion
splitBVarTests = do
  y @=? 3 * 7 + 5 * 11
  13 |*> dy @=? RecordValue (MyRecord (13 * 3) (13 * 5))
  dy <*| RecordValue (MyRecord 17 19) @=? 3 * 17 + 5 * 19
 where
  x :: RecordValue MyRecord (MonoLeaf Int)
  x = RecordValue (MyRecord 7 11)
  (y, dy) = unDFun (BVar.ad myBVarFun) x

myFunT :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => a -> MyRecord (MonoLeaf a)
myFunT x = MyRecord (x |* 3) (x |* 5)

myBVarFunT ::
  (IsScalar a, HasBuilder a, FrobeniusAlgebra a, HasGradRepr (MonoLeaf a)) =>
  BVar r a -> BVar r (RecordValue MyRecord (MonoLeaf a))
myBVarFunT = joinMyRecord . myFunT

joinMyRecord ::
  forall r a.
  (HasGradRepr (MonoLeaf a)) =>
  MyRecord (MonoLeaf (BVar r a)) ->
  BVar r (RecordValue MyRecord (MonoLeaf a))
joinMyRecord = joinBVarRecord

joinBVarTests :: Assertion
joinBVarTests = do
  y @=? MyRecord (3 * 11) (5 * 11)
  RecordValue (MyRecord 13 17) |*> dy @=? 3 * 13 + 5 * 17
  dy <*| 19 @=? RecordValue (MyRecord (3 * 19) (5 * 19))
 where
  x :: Int
  x = 11
  (RecordValue y, dy) = unDFun (BVar.ad myBVarFunT) x

toListTest :: Assertion
toListTest = do
  let x :: Record (ConstSym1 Int) @@ 'ShR MyRecord
      x = MyRecord 3 4
  toListRecordEx (sing @('ShR MyRecord)) x @=? [3, 4]

recordTests :: SpecWith ()
recordTests = describe "Record" $ do
  it "derives optional field names" testFieldNames
  it "fill" (testFill1 >> testFill2)
  it "plus" testPlus
  it "getter" testField
  it "split" splitBVarTests
  it "join" joinBVarTests
  it "fold" toListTest
  builderTests
