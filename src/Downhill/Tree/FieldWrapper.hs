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
  MultiplicativeActionField
)
where

import Data.Singletons (SingI, type (@@), type (~>))
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