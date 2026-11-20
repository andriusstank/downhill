{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Downhill.Tree.At (
    At (..),
    AdditiveRepr,
    EqRepr,
    ShowRepr,
    SubtractiveRepr,
    MultiplicativeRepr,
    MultiplicativeActionShape,
    MultiplicativeActionRepr,
    MonoidRepr,
    VSpaceRepr,
    InnerSpaceField, InnerSpaceShape,
    ManifoldShape,
    ManifoldRepr,
    DenseAt (..),
    HasBuilderRepr,
    ShowAllShapeCases,
    GenericAr(..)
)
where

import Data.Singletons (SingI, type (@@), type (~>), sing)
import Downhill.Linear.Builder (HasBuilder (..), maybeToMonoid)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold, Tang)
import Downhill.VSpace (VSpace, InnerSpace)
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)
import Data.Constraint (Dict (Dict))
import Downhill.Tree.Representation (ReprDualOf, ReprTang, ReprGrad, AllConstrained(..), ReprScalar)

-- | At newtype is obsolete
newtype At repr sh = At {unAt :: repr @@ sh}

class (forall sh. (SingI sh) => Eq (At repr sh)) => EqRepr repr
instance (forall sh. (SingI sh) => Eq (At repr sh)) => EqRepr repr

class (forall sh. (SingI sh) => Show (At repr sh)) => ShowRepr repr
instance (forall sh. (SingI sh) => Show (At repr sh)) => ShowRepr repr

class (forall sh. (SingI sh) => Additive (At repr sh)) => AdditiveRepr repr
instance (forall sh. (SingI sh) => Additive (At repr sh)) => AdditiveRepr repr

class (forall sh. (SingI sh) => Subtractive (At repr sh)) => SubtractiveRepr repr
instance (forall sh. (SingI sh) => Subtractive (At repr sh)) => SubtractiveRepr repr

class (forall sh. (SingI sh) => Multiplicative (At repr sh)) => MultiplicativeRepr repr
instance (forall sh. (SingI sh) => Multiplicative (At repr sh)) => MultiplicativeRepr repr

class (forall sh. (SingI sh) => Monoid (At f sh)) => MonoidRepr f
instance (forall sh. (SingI sh) => Monoid (At f sh)) => MonoidRepr f

class
    ( MultiplicativeAction (At field sh)
    , Scalar (At field sh) ~ ReprScalar field
    ) =>
    MultiplicativeActionShape field sh
instance
    ( MultiplicativeAction (At field sh)
    , Scalar (At field sh) ~ ReprScalar field
    ) =>
    MultiplicativeActionShape field sh

type MultiplicativeActionRepr :: (k ~> Type) -> Constraint
class
    ( Multiplicative (ReprScalar field)
    , FromInteger (ReprScalar field) 
    , forall sh. (SingI sh) => MultiplicativeActionShape field sh
    ) =>
    MultiplicativeActionRepr field
instance
    ( Multiplicative (ReprScalar field)
    , FromInteger (ReprScalar field)
    , forall sh. (SingI sh) => MultiplicativeActionShape field sh
    ) =>
    MultiplicativeActionRepr field

type instance DualOf (At field sh) = At (ReprDualOf field) sh

class
    ( VSpace (At field sh)
    , DualOf (At field sh) ~ At (ReprDualOf field) sh
    ) =>
    VSpaceShape field sh
instance
    ( VSpace (At field sh)
    , DualOf (At field sh) ~ At (ReprDualOf field) sh
    ) =>
    VSpaceShape field sh

class
    ( SubtractiveRepr field
    , SubtractiveRepr (ReprDualOf field)
    , Subtractive (ReprScalar field)
    , MultiplicativeActionRepr field
    , MultiplicativeActionRepr (ReprDualOf field)
    , ReprScalar (ReprDualOf field) ~ ReprScalar field
    , DualOf (ReprScalar field) ~ ReprScalar field
    , ReprDualOf (ReprDualOf field) ~ field
    , Scalar (ReprScalar field) ~ ReprScalar field
    , (forall sh. (SingI sh) => VSpaceShape field sh)
    ) =>
    VSpaceRepr field
instance
    ( SubtractiveRepr field
    , SubtractiveRepr (ReprDualOf field)
    , Subtractive (ReprScalar field)
    , MultiplicativeActionRepr field
    , MultiplicativeActionRepr (ReprDualOf field)
    , ReprScalar (ReprDualOf field) ~ ReprScalar field
    , DualOf (ReprScalar field) ~ ReprScalar field
    , ReprDualOf (ReprDualOf field) ~ field
    , Scalar (ReprScalar field) ~ ReprScalar field
    , forall sh. (SingI sh) => VSpaceShape field sh
    ) =>
    VSpaceRepr field

class
    ( InnerSpace (At field sh)
    ) =>
    InnerSpaceShape field sh
instance
    ( InnerSpace (At field sh)
    ) =>
    InnerSpaceShape field sh

class (forall sh. (SingI sh) => InnerSpaceShape field sh, VSpaceRepr field) => InnerSpaceField field
instance (forall sh. (SingI sh) => InnerSpaceShape field sh, VSpaceRepr field) => InnerSpaceField field

class
    ( Manifold (At field sh)
    , Tang (At field sh) ~ At (ReprTang field) sh
    ) =>
    ManifoldShape field sh
instance
    ( Manifold (At field sh)
    , Tang (At field sh) ~ At (ReprTang field) sh
    ) =>
    ManifoldShape field sh

class
    ( VSpaceRepr (ReprTang field)
    , ReprTang (ReprGrad field) ~ ReprGrad field
    , ReprTang (ReprTang field) ~ ReprTang field
    , Tang (ReprScalar (ReprTang field)) ~ ReprScalar (ReprTang field)
    , forall sh. (SingI sh) => ManifoldShape field sh
    ) =>
    ManifoldRepr field
instance
    ( VSpaceRepr (ReprTang field)
    , ReprTang (ReprDualOf (ReprTang field)) ~ ReprDualOf (ReprTang field)
    , ReprTang (ReprTang field) ~ ReprTang field
    , Tang (ReprScalar (ReprTang field)) ~ ReprScalar (ReprTang field)
    , forall sh. (SingI sh) => ManifoldShape field sh
    ) =>
    ManifoldRepr field

newtype DenseAt field sh = DenseAt {unDenseFieldWrapper :: field @@ sh}

deriving via (At field sh) instance (Additive (At field sh)) => Additive (DenseAt field sh)

instance (Additive (At field sh)) => HasBuilder (DenseAt field sh) where
    type VecBuilder (DenseAt field sh) = Maybe (Sum (At field sh))
    sumBuilder = DenseAt . unAt . getSum . maybeToMonoid
    identityBuilder = Just . Sum . At . unDenseFieldWrapper

class (forall sh. (SingI sh) => HasBuilder (At f sh)) => HasBuilderRepr f
instance (forall sh. (SingI sh) => HasBuilder (At f sh)) => HasBuilderRepr f

type instance Tang (At field sh) = At (ReprTang field) sh

deriving via (a :: Type) instance Additive a => Additive (At (ConstSym1 a) sh)
deriving via (a :: Type) instance Subtractive a => Subtractive (At (ConstSym1 a) sh)
deriving via (a :: Type) instance Multiplicative a => Multiplicative (At (ConstSym1 a) sh)
deriving via (a :: Type) instance Semigroup a => Semigroup (At (ConstSym1 a) sh)
deriving via (a :: Type) instance Monoid a => Monoid (At (ConstSym1 a) sh)
instance (HasBuilder a) => HasBuilder (At (ConstSym1 a) sh) where
    type VecBuilder (At (ConstSym1 a) sh) = VecBuilder a
    sumBuilder = At . sumBuilder
    identityBuilder = identityBuilder . unAt
deriving via (a :: Type) instance Eq a => Eq (At (ConstSym1 a) sh)
deriving via (a :: Type) instance Show a => Show (At (ConstSym1 a) sh)
deriving via (a :: Type) instance FromInteger a => FromInteger (At (ConstSym1 a) sh)
deriving via (a :: Type) instance MultiplicativeAction a => MultiplicativeAction (At (ConstSym1 a) sh)
deriving via (a :: Type) instance VSpace a => VSpace (At (ConstSym1 a) sh)
deriving via (a :: Type) instance Manifold a => Manifold (At (ConstSym1 a) sh)

newtype GenericAr field sh = GenericAr (field @@ sh)



instance (AllConstrained Additive field, SingI sh) => Additive (GenericAr field sh) where
    zero = case allConstrained @Additive @field (sing @sh) of
        Dict -> GenericAr zero
    GenericAr x + GenericAr y  = case allConstrained @Additive @field (sing @sh) of
        Dict -> GenericAr (x+y)

instance (AllConstrained Additive field, AllConstrained Subtractive field, SingI sh) => Subtractive (GenericAr field sh) where
    negate = case allConstrained @Subtractive @field (sing @sh) of
        Dict -> \(GenericAr x) -> GenericAr (negate x)
    GenericAr x - GenericAr y  = case allConstrained @Subtractive @field (sing @sh) of
        Dict -> GenericAr (x-y)

type family ShowAllShapeCases (f :: k ~> Type) :: Constraint
