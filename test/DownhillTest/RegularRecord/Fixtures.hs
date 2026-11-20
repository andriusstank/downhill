{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DownhillTest.RegularRecord.Fixtures (
    LeafShape (..), SLeafShape (..), VarLeaf,
    MyRecordA (..), MyRecordAFields, MyRecordB (..), MyRecordBFields,
    MyTupleRecord (..), MyNestedNewtype (..), MyScalarNewtype (..),
    SplitLeafShape (..), SSplitLeafShape (..), SplitLeaf,
    SplitRecordA (..), SplitTupleRecord (..), SplitRecordB (..),
    SplitRecordTree, splitRecordValues, splitRecordTree, splitRecordTreeValues,
) where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, Sing, SingI (..), type (@@), type (~>))
import Data.Vector.Sized (Vector)
import Downhill.BVar (BVar)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold, Tang)
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.RegularRecord (IsRegularRecord, RecordTree (..), RegularTreeShape (..))
import Downhill.Tree.Representation (
    AllConstrained (..), PointwiseManifold (..), PointwiseVSpace (..),
    ReprDualOf, ReprScalar, ReprTang,
 )
import Downhill.VSpace (VSpace)
import GHC.TypeLits (KnownNat, Nat)
import NumHask.Prelude

data LeafShape = LshScalar | LshVector Nat

data SLeafShape (sh :: LeafShape) where
    SLshScalar :: SLeafShape 'LshScalar
    SLshVector :: (KnownNat n) => SLeafShape ('LshVector n)

type instance Sing = SLeafShape

instance SingI 'LshScalar where
    sing = SLshScalar

instance (KnownNat n) => SingI ('LshVector n) where
    sing = SLshVector

data VarLeaf (a :: Type) :: LeafShape ~> Type

type instance Apply (VarLeaf a) 'LshScalar = a
type instance Apply (VarLeaf a) ('LshVector n) = Vector n a

data MyRecordA (leaf :: LeafShape ~> Type) = MyRecordA
    { scalarField :: leaf @@ 'LshScalar
    , vectorField :: leaf @@ 'LshVector 2
    }
    deriving stock (Generic)

deriving instance Eq (MyRecordA (VarLeaf Int))
deriving instance Show (MyRecordA (VarLeaf Int))

type MyRecordAFields =
    '[ '("scalarField", 'RegularTreeShapeLeaf 'LshScalar)
     , '("vectorField", 'RegularTreeShapeLeaf ('LshVector 2))
     ]

instance IsRegularRecord MyRecordA

data MyRecordB (leaf :: LeafShape ~> Type) = MyRecordB
    { scalarFieldB :: leaf @@ 'LshScalar
    , subrecordField :: MyRecordA leaf
    }
    deriving stock (Generic)

deriving instance Eq (MyRecordB (VarLeaf Int))
deriving instance Show (MyRecordB (VarLeaf Int))

type MyRecordBFields =
    '[ '("scalarFieldB", 'RegularTreeShapeLeaf 'LshScalar)
     , '("subrecordField", 'RegularTreeShapeSubtree MyRecordA)
     ]

instance IsRegularRecord MyRecordB

data MyTupleRecord (leaf :: LeafShape ~> Type) = MyTupleRecord (leaf @@ 'LshScalar) (leaf @@ 'LshVector 2)
    deriving stock (Generic)

deriving instance Eq (MyTupleRecord (VarLeaf Int))
deriving instance Show (MyTupleRecord (VarLeaf Int))

newtype MyNestedNewtype (leaf :: LeafShape ~> Type) = MyNestedNewtype (MyRecordA leaf)
    deriving stock (Generic)

deriving instance Eq (MyNestedNewtype (VarLeaf Int))
deriving instance Show (MyNestedNewtype (VarLeaf Int))

newtype MyScalarNewtype (leaf :: LeafShape ~> Type) = MyScalarNewtype (leaf @@ 'LshScalar)
    deriving stock (Generic)

deriving instance Eq (MyScalarNewtype (VarLeaf Int))
deriving instance Show (MyScalarNewtype (VarLeaf Int))

instance IsRegularRecord MyTupleRecord

instance IsRegularRecord MyNestedNewtype

instance IsRegularRecord MyScalarNewtype

data SplitLeafShape = SplitScalar | SplitVector

data SSplitLeafShape (sh :: SplitLeafShape) where
    SSplitScalar :: SSplitLeafShape 'SplitScalar
    SSplitVector :: SSplitLeafShape 'SplitVector

type instance Sing = SSplitLeafShape

instance SingI 'SplitScalar where
    sing = SSplitScalar
instance SingI 'SplitVector where
    sing = SSplitVector

data SplitLeaf (v :: Type) :: SplitLeafShape ~> Type

type instance Apply (SplitLeaf v) 'SplitScalar = (Scalar v)
type instance Apply (SplitLeaf v) 'SplitVector = v

type instance ReprScalar (SplitLeaf v) = Scalar v
type instance ReprTang (SplitLeaf v) = SplitLeaf (Tang v)
type instance ReprDualOf (SplitLeaf v) = SplitLeaf (DualOf v)

instance (c (Scalar a), c a) => AllConstrained c (SplitLeaf a) where
    allConstrained SSplitScalar = Dict
    allConstrained SSplitVector = Dict

instance (VSpace (Scalar a), VSpace a) => PointwiseVSpace (SplitLeaf a) where
    vspaceDict SSplitScalar = Dict
    vspaceDict SSplitVector = Dict

instance (Manifold a, Manifold (Scalar a), Tang (Scalar a) ~ Scalar (Tang a)) => PointwiseManifold (SplitLeaf a) where
    manifoldDict SSplitScalar = Dict
    manifoldDict SSplitVector = Dict

data SplitRecordA (leaf :: SplitLeafShape ~> Type) = SplitRecordA
    { splitFieldA :: leaf @@ 'SplitScalar
    , splitFieldB :: leaf @@ 'SplitScalar
    }
    deriving stock (Generic)

deriving instance Eq (SplitRecordA (SplitLeaf Int))
deriving instance Show (SplitRecordA (SplitLeaf Int))

data SplitTupleRecord (leaf :: SplitLeafShape ~> Type)
    = SplitTupleRecord
        (leaf @@ 'SplitScalar)
        (leaf @@ 'SplitScalar)
    deriving stock (Generic)

deriving instance Eq (SplitTupleRecord (SplitLeaf Int))
deriving instance Show (SplitTupleRecord (SplitLeaf Int))

data SplitRecordB (leaf :: SplitLeafShape ~> Type) = SplitRecordB
    { splitNamedRecord :: SplitRecordA leaf
    , splitTupleRecord :: SplitTupleRecord leaf
    , splitFieldC :: leaf @@ 'SplitScalar
    }
    deriving stock (Generic)

instance IsRegularRecord SplitRecordA

instance IsRegularRecord SplitTupleRecord

instance IsRegularRecord SplitRecordB

splitRecordValues ::
    SplitRecordB (SplitLeaf Int) ->
    (Int, Int, Int, Int, Int)
splitRecordValues
    ( SplitRecordB
            (SplitRecordA fieldA fieldB)
            (SplitTupleRecord fieldC fieldD)
            fieldG
        ) =
        ( fieldA
        , fieldB
        , fieldC
        , fieldD
        , fieldG
        )

type SplitRecordTree = RecordTree (SplitLeaf Int) ('RegularTreeShapeSubtree SplitRecordB)

splitRecordTree :: Int -> Int -> Int -> Int -> Int -> SplitRecordTree
splitRecordTree fieldA fieldB fieldC fieldD fieldG =
    RecordTree
        ( SplitRecordB
            (SplitRecordA fieldA fieldB)
            (SplitTupleRecord fieldC fieldD)
            fieldG
        )

splitRecordTreeValues :: SplitRecordTree -> (Int, Int, Int, Int, Int)
splitRecordTreeValues = splitRecordValues . unRecordTree

instance BVarRepr (SplitLeaf v) where
    type ReprBVar r (SplitLeaf v) = SplitLeaf (BVar r v)
    splitBVarField SSplitScalar = id
    splitBVarField SSplitVector = id
    joinBVarField SSplitScalar = id
    joinBVarField SSplitVector = id

