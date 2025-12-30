{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Record (recordTests)
where

import Data.Singletons (TyCon1, type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.BVar (BVar (..), unDFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder (..))
import Downhill.Linear.Fun ((<*|), (|*>))
import Downhill.Manifold (Grad, IsScalar, Manifold (..), Tang)
import Downhill.Tree.FieldWrapper (FieldWrapper (..))
import Downhill.Tree.Record (
  FieldGetter,
  IsRecord (..),
  Record,
  RecordBuilder,
  ShR (..),
  fieldBuilder,
  joinBVarRecord,
  recordField,
  splitBVarRecord,
 )
import DownhillTest.MonoLeaf (Field, MonoLeaf)
import GHC.Records (getField)
import NumHask.Prelude hiding (Field)
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)
import Downhill.Tree.BVar (BVarField(..))

data MyRecord (f :: () ~> Type) = MyRecord
  { fieldA :: Field f
  , fieldB :: Field f
  }
  deriving (Generic)

instance IsRecord MyRecord
deriving instance (Eq a) => Eq (MyRecord (MonoLeaf a))
deriving instance (Show a) => Show (MyRecord (MonoLeaf a))

testFill1 :: Assertion
testFill1 = fillRecordEx f @=? (MyRecord 3 3 :: MyRecord (MonoLeaf Int))
 where
  f :: STuple0 sh -> MonoLeaf Int @@ sh
  f STuple0 = 3

testFill2 :: Assertion
testFill2 = fillRecordEx f @=? (MyRecord 1 1 :: MyRecord (MonoLeaf Int))
 where
  f :: STuple0 sh -> MonoLeaf Int @@ sh
  f STuple0 = one

deriving instance Show (FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord))
deriving instance Eq (FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord))

testPlus :: Assertion
testPlus = FieldWrapper (MyRecord 8 10) @=? (x + y)
 where
  x, y :: FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord)
  x = FieldWrapper (MyRecord 3 4)
  y = FieldWrapper (MyRecord 5 6)

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
  let mkRecord :: Int -> Int -> FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord)
      mkRecord x y = FieldWrapper (MyRecord x y)
      fs :: MyRecord (TyCon1 (FieldGetter MyRecord))
      fs = recordGettersEx
      mkBuilder :: FieldGetter MyRecord '() -> Int -> RecordBuilder ('ShR MyRecord) (MonoLeaf Int)
      -- mkBuilder field value = fieldBuilder field (Just (Sum (FieldWrapper value)))
      mkBuilder field value = fieldBuilder field (identityBuilder value)
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
  (Manifold a, HasBuilder (Grad a), HasBuilder (Tang a)) =>
  BVar r (FieldWrapper (Record (MonoLeaf a)) ('ShR MyRecord)) ->
  FieldWrapper (Record (MonoLeaf (BVar r a))) ('ShR MyRecord)
splitMyRecord = splitBVarRecord splitField

myFun :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => MyRecord (MonoLeaf a) -> a
myFun x = (x.fieldA |* 3) + (x.fieldB |* 5)

myBVarFun ::
  (IsScalar a, HasBuilder a, FromInteger a) =>
  BVar r (FieldWrapper (Record (MonoLeaf a)) ('ShR MyRecord)) -> BVar r a
myBVarFun = myFun . unFieldWrapper . splitMyRecord

splitBVarTests :: Assertion
splitBVarTests = do
  y @=? 3 * 7 + 5 * 11
  13 |*> dy @=? FieldWrapper (MyRecord (13 * 3) (13 * 5))
  dy <*| FieldWrapper (MyRecord 17 19) @=? 3 * 17 + 5 * 19
 where
  x :: FieldWrapper (Record (MonoLeaf Int)) ('ShR MyRecord)
  x = FieldWrapper (MyRecord 7 11)
  (y, dy) = unDFun (BVar.ad myBVarFun) x

myFunT :: (MultiplicativeAction a, FromInteger (Scalar a), Additive a) => a -> MyRecord (MonoLeaf a)
myFunT x = MyRecord (x |* 3) (x |* 5)

myBVarFunT ::
  (IsScalar a, HasBuilder a, FromInteger a) =>
  BVar r a -> BVar r (FieldWrapper (Record (MonoLeaf a)) ('ShR MyRecord))
myBVarFunT = joinMyRecord . FieldWrapper . myFunT

joinMyRecord ::
  forall r a.
  (Manifold a, HasBuilder (Grad a), HasBuilder (Tang a)) =>
  FieldWrapper (Record (MonoLeaf (BVar r a))) ('ShR MyRecord) ->
  BVar r (FieldWrapper (Record (MonoLeaf a)) ('ShR MyRecord))
joinMyRecord = joinBVarRecord joinField

joinBVarTests :: Assertion
joinBVarTests = do
  y @=? MyRecord (3 * 11) (5 * 11)
  FieldWrapper (MyRecord 13 17) |*> dy @=? 3 * 13 + 5 * 17
  dy <*| 19 @=? FieldWrapper (MyRecord (3 * 19) (5 * 19))
 where
  x :: Int
  x = 11
  (FieldWrapper y, dy) = unDFun (BVar.ad myBVarFunT) x

recordTests :: SpecWith ()
recordTests = describe "Record" $ do
  it "fill" (testFill1 >> testFill2)
  it "plus" testPlus
  it "getter" testField
  it "split" splitBVarTests
  it "join" joinBVarTests
  builderTests
