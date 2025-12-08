{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Tree.FieldWrapper (
    FieldWrapper (..),
    AdditiveField,
    SubtractiveField,
    MultiplicativeField,
    FieldScalar,
    MultiplicativeActionFieldSh,
    MultiplicativeActionField,
    VSpaceField,
    FieldDualOf,
    FieldTang,
    ManifoldFieldSh,
    ManifoldField,
    DenseFieldWrapper (..),
)
where

import Data.Singletons (SingI, type (@@), type (~>))
import Downhill.Linear.Builder (HasBuilder (..), maybeToMonoid)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (Tang))
import Downhill.VSpace (VSpace)
import NumHask.Prelude

newtype FieldWrapper field sh = FieldWrapper {unFieldWrapper :: field @@ sh}

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
    , (forall sh. (SingI sh) => VSpaceFieldSh field sh)
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
    , forall sh. (SingI sh) => VSpaceFieldSh field sh
    ) =>
    VSpaceField field

type family FieldTang (a :: k ~> Type) :: k ~> Type

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
    , forall sh. (SingI sh) => ManifoldFieldSh field sh
    ) =>
    ManifoldField field
instance
    ( VSpaceField (FieldTang field)
    , forall sh. (SingI sh) => ManifoldFieldSh field sh
    ) =>
    ManifoldField field

newtype DenseFieldWrapper field sh = DenseFieldWrapper {unDenseFieldWrapper :: field @@ sh}

deriving via (FieldWrapper field sh) instance (Additive (FieldWrapper field sh)) => Additive (DenseFieldWrapper field sh)

instance (Additive (FieldWrapper field sh)) => HasBuilder (DenseFieldWrapper field sh) where
    type VecBuilder (DenseFieldWrapper field sh) = Maybe (Sum (FieldWrapper field sh))
    sumBuilder = DenseFieldWrapper . unFieldWrapper . getSum . maybeToMonoid
    identityBuilder = Just . Sum . FieldWrapper . unDenseFieldWrapper
