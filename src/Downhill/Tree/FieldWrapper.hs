{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Downhill.Tree.FieldWrapper (
    FieldWrapper (..),
    AdditiveField,
    EqField,
    SubtractiveField,
    MultiplicativeField,
    FieldScalar,
    MultiplicativeActionFieldSh,
    MultiplicativeActionField,
    VSpaceField,
    FieldDualOf,
    FieldTang,
    FieldGrad,
    ManifoldFieldSh,
    ManifoldField,
    DenseFieldWrapper (..),
    HasBuilderField
)
where

import Data.Singletons (SingI, type (@@), type (~>))
import Downhill.Linear.Builder (HasBuilder (..), maybeToMonoid)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold, Tang)
import Downhill.VSpace (VSpace, InnerSpace, CScalar)
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)

newtype FieldWrapper field sh = FieldWrapper {unFieldWrapper :: field @@ sh}

class (forall sh. (SingI sh) => Eq (FieldWrapper f sh)) => EqField f
instance (forall sh. (SingI sh) => Eq (FieldWrapper f sh)) => EqField f

class (forall sh. (SingI sh) => Additive (FieldWrapper f sh)) => AdditiveField f
instance (forall sh. (SingI sh) => Additive (FieldWrapper f sh)) => AdditiveField f

class (forall sh. (SingI sh) => Subtractive (FieldWrapper f sh)) => SubtractiveField f
instance (forall sh. (SingI sh) => Subtractive (FieldWrapper f sh)) => SubtractiveField f

class (forall sh. (SingI sh) => Multiplicative (FieldWrapper f sh)) => MultiplicativeField f
instance (forall sh. (SingI sh) => Multiplicative (FieldWrapper f sh)) => MultiplicativeField f

type family FieldScalar (field :: sh ~> Type) :: Type

class
    ( MultiplicativeAction (FieldWrapper field sh)
    , Scalar (FieldWrapper field sh) ~ FieldScalar field
    ) =>
    MultiplicativeActionFieldSh field sh
instance
    ( MultiplicativeAction (FieldWrapper field sh)
    , Scalar (FieldWrapper field sh) ~ FieldScalar field
    ) =>
    MultiplicativeActionFieldSh field sh

type MultiplicativeActionField :: (k ~> Type) -> Constraint
class
    ( Multiplicative (FieldScalar field)
    , forall sh. (SingI sh) => MultiplicativeActionFieldSh field sh
    ) =>
    MultiplicativeActionField field
instance
    ( Multiplicative (FieldScalar field)
    , forall sh. (SingI sh) => MultiplicativeActionFieldSh field sh
    ) =>
    MultiplicativeActionField field

type family FieldDualOf (a :: k ~> Type) :: k ~> Type

type instance DualOf (FieldWrapper field sh) = FieldWrapper (FieldDualOf field) sh

class
    ( VSpace (FieldWrapper field sh)
    , DualOf (FieldWrapper field sh) ~ FieldWrapper (FieldDualOf field) sh
    ) =>
    VSpaceFieldSh field sh
instance
    ( VSpace (FieldWrapper field sh)
    , DualOf (FieldWrapper field sh) ~ FieldWrapper (FieldDualOf field) sh
    ) =>
    VSpaceFieldSh field sh

class
    ( SubtractiveField field
    , SubtractiveField (FieldDualOf field)
    , Subtractive (FieldScalar field)
    , MultiplicativeActionField field
    , MultiplicativeActionField (FieldDualOf field)
    , FieldScalar (FieldDualOf field) ~ FieldScalar field
    , DualOf (FieldScalar field) ~ FieldScalar field
    , FieldDualOf (FieldDualOf field) ~ field
    , Scalar (FieldScalar field) ~ FieldScalar field
    , InnerSpace (FieldScalar field)
    , (forall sh. (SingI sh) => VSpaceFieldSh field sh)
    , CScalar (FieldScalar field)
    ) =>
    VSpaceField field
instance
    ( SubtractiveField field
    , SubtractiveField (FieldDualOf field)
    , Subtractive (FieldScalar field)
    , MultiplicativeActionField field
    , MultiplicativeActionField (FieldDualOf field)
    , FieldScalar (FieldDualOf field) ~ FieldScalar field
    , DualOf (FieldScalar field) ~ FieldScalar field
    , FieldDualOf (FieldDualOf field) ~ field
    , Scalar (FieldScalar field) ~ FieldScalar field
    , InnerSpace (FieldScalar field)
    , forall sh. (SingI sh) => VSpaceFieldSh field sh
    , CScalar (FieldScalar field)
    ) =>
    VSpaceField field

type family FieldTang (a :: k ~> Type) :: k ~> Type

type FieldGrad f = FieldDualOf (FieldTang f)

class
    ( Manifold (FieldWrapper field sh)
    , Tang (FieldWrapper field sh) ~ FieldWrapper (FieldTang field) sh
    ) =>
    ManifoldFieldSh field sh
instance
    ( Manifold (FieldWrapper field sh)
    , Tang (FieldWrapper field sh) ~ FieldWrapper (FieldTang field) sh
    ) =>
    ManifoldFieldSh field sh

class
    ( VSpaceField (FieldTang field)
    , FieldTang (FieldDualOf field) ~ FieldDualOf field
    , FieldTang (FieldTang field) ~ FieldTang field
    , Tang (FieldScalar (FieldTang field)) ~ FieldScalar (FieldTang field)
    , forall sh. (SingI sh) => ManifoldFieldSh field sh
    ) =>
    ManifoldField field
instance
    ( VSpaceField (FieldTang field)
    , FieldTang (FieldDualOf field) ~ FieldDualOf field
    , FieldTang (FieldTang field) ~ FieldTang field
    , Tang (FieldScalar (FieldTang field)) ~ FieldScalar (FieldTang field)
    , forall sh. (SingI sh) => ManifoldFieldSh field sh
    ) =>
    ManifoldField field

newtype DenseFieldWrapper field sh = DenseFieldWrapper {unDenseFieldWrapper :: field @@ sh}

deriving via (FieldWrapper field sh) instance (Additive (FieldWrapper field sh)) => Additive (DenseFieldWrapper field sh)

instance (Additive (FieldWrapper field sh)) => HasBuilder (DenseFieldWrapper field sh) where
    type VecBuilder (DenseFieldWrapper field sh) = Maybe (Sum (FieldWrapper field sh))
    sumBuilder = DenseFieldWrapper . unFieldWrapper . getSum . maybeToMonoid
    identityBuilder = Just . Sum . FieldWrapper . unDenseFieldWrapper

class (forall sh. (SingI sh) => HasBuilder (FieldWrapper f sh)) => HasBuilderField f
instance (forall sh. (SingI sh) => HasBuilder (FieldWrapper f sh)) => HasBuilderField f

type instance Tang (FieldWrapper field sh) = FieldWrapper (FieldTang field) sh

deriving via (a :: Type) instance Additive a => Additive (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance Subtractive a => Subtractive (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance Multiplicative a => Multiplicative (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance Semigroup a => Semigroup (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance Monoid a => Monoid (FieldWrapper (ConstSym1 a) sh)
instance (HasBuilder a) => HasBuilder (FieldWrapper (ConstSym1 a) sh) where
    type VecBuilder (FieldWrapper (ConstSym1 a) sh) = VecBuilder a
    sumBuilder = FieldWrapper . sumBuilder
    identityBuilder = identityBuilder . unFieldWrapper
deriving via (a :: Type) instance Eq a => Eq (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance FromInteger a => FromInteger (FieldWrapper (ConstSym1 a) sh)
type instance FieldTang (ConstSym1 a) = ConstSym1 (Tang a)
type instance FieldDualOf (ConstSym1 a) = ConstSym1 (DualOf a)
type instance FieldScalar (ConstSym1 a) = Scalar a
deriving via (a :: Type) instance MultiplicativeAction a => MultiplicativeAction (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance VSpace a => VSpace (FieldWrapper (ConstSym1 a) sh)
deriving via (a :: Type) instance Manifold a => Manifold (FieldWrapper (ConstSym1 a) sh)

