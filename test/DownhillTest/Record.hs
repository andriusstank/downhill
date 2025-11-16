{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.Record (recordTests)
where

import Downhill.Tree.FieldWrapper (FieldWrapper (..))
import Downhill.Tree.Record (IsRecord (fillRecord), fillRecordI)
import NumHask.Prelude hiding (Field)
import Prelude.Singletons (Apply, STuple0 (STuple0), SingI (sing), type (@@), type (~>))
import Test.HUnit (Assertion, (@=?))
import Test.Hspec (SpecWith, describe, it)

data MonoLeaf a :: () ~> Type
type instance Apply (MonoLeaf a) '() = a

instance (SingI sh, Multiplicative a) => Multiplicative (FieldWrapper (MonoLeaf a) sh) where
    one = case sing @sh of
        STuple0 -> FieldWrapper one
    FieldWrapper x * FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x*y)

type Field f = f @@ '()

data MyRecord (f :: () ~> Type) = MyRecord (Field f) (Field f)
    deriving (Generic)

instance IsRecord MyRecord
deriving instance (Eq a) => Eq (MyRecord (MonoLeaf a))
deriving instance (Show a) => Show (MyRecord (MonoLeaf a))

testFill1 :: Assertion
testFill1 = fillRecord f @=? (MyRecord 3 3 :: MyRecord (MonoLeaf Int))
  where
    f :: STuple0 sh -> MonoLeaf Int @@ sh
    f STuple0 = 3

testFill2 :: Assertion
testFill2 = fillRecordI f @=? (MyRecord 1 1 :: MyRecord (MonoLeaf Int))
  where
    f :: SingI sh => FieldWrapper (MonoLeaf Int) sh
    f = one

recordTests :: SpecWith ()
recordTests = describe "Record" $ do
    it "fill" (testFill1 >> testFill2)
