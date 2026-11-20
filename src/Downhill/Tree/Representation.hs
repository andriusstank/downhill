{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | Vocabulary for record and tree representations.

A representation is a type-level function @repr :: sh ~> Type@.  It assigns a
concrete Haskell type to every shape @sh@.  For example, one representation
may map a scalar shape to a scalar value and a vector shape to a sized vector;
another representation may map the same shapes to tangent values, dual values,
builders, or variables.

Use this module to define representations for domain-specific leaf shapes.
A leaf shape can be a small type such as scalar-or-vector, or something more
structural such as array dimensions or array rank.  To make such leaves usable
by the tree machinery, provide instances from this module for the
representation: which constraints hold for all shapes, what its scalar type
is, and how to compute tangent and dual representations.
-}
module Downhill.Tree.Representation (
    AllConstrained (..),
    AdditiveRepr,
    SubtractiveRepr,
    BuilderRepr,
    ReprScalar,
    MultiplicativeActionOver,
    MultiplicativeActionRepr,
    ReprDualOf,
    ReprTang,
    ReprGrad,
    PointwiseVSpace (..),
    VSpaceRepr,
    InnerSpaceRepr,
    innerSpaceReprDict,
    PointwiseMetric (..),
    PointwiseManifold (..),
    ManifoldRepr,
    manifoldReprDict,
    HasGradRepr,
    hasGradReprDict,
)
where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Sing, type (@@), type (~>))
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf, Metric)
import Downhill.Manifold (Grad, HasGrad, Manifold, Tang)
import Downhill.VSpace (InnerSpace, VSpace)
import NumHask.Prelude
import Prelude.Singletons (ConstSym1)

{- | Evidence that every case of a representation satisfies a constraint.

@'AllConstrained' Eq repr@ means that, for any known shape @sh@, the concrete
field type @repr @@ sh@ has an 'Eq' instance.  Generic record code uses the
supplied singleton to recover the appropriate dictionary for each field.
-}
class AllConstrained c field where
    allConstrained :: forall sh. Sing sh -> Dict (c (field @@ sh))

-- | A representation whose every shape is additive.
class (AllConstrained Additive repr) => AdditiveRepr repr

instance (AllConstrained Additive repr) => AdditiveRepr repr

-- | A representation whose every shape is subtractive.
class
    ( AdditiveRepr repr
    , AllConstrained Subtractive repr
    ) =>
    SubtractiveRepr repr

instance
    ( AdditiveRepr repr
    , AllConstrained Subtractive repr
    ) =>
    SubtractiveRepr repr

-- | A representation whose every shape has a sparse vector builder.
class
    ( AdditiveRepr repr
    , AllConstrained HasBuilder repr
    ) =>
    BuilderRepr repr

instance
    ( AdditiveRepr repr
    , AllConstrained HasBuilder repr
    ) =>
    BuilderRepr repr

{- | Scalar type shared by values in a representation.

This is used by vector-space-like operations where every field of a record
acts over the same scalar type.
-}
type family ReprScalar (field :: sh ~> Type) :: Type

{- | 'MultiplicativeAction' specialized to a chosen scalar type.

This is useful with 'AllConstrained': it lets a record require that every
field supports scalar multiplication over the representation scalar.
-}
class
    ( MultiplicativeAction v
    , Scalar v ~ a
    ) =>
    MultiplicativeActionOver a v

instance
    ( MultiplicativeAction v
    , Scalar v ~ a
    ) =>
    MultiplicativeActionOver a v

{- | A representation whose every shape supports scalar multiplication by the
representation scalar.
-}
class
    ( Multiplicative (ReprScalar repr)
    , FromInteger (ReprScalar repr)
    , AllConstrained (MultiplicativeActionOver (ReprScalar repr)) repr
    ) =>
    MultiplicativeActionRepr repr

instance
    ( Multiplicative (ReprScalar repr)
    , FromInteger (ReprScalar repr)
    , AllConstrained (MultiplicativeActionOver (ReprScalar repr)) repr
    ) =>
    MultiplicativeActionRepr repr

{- | Representation of dual values.

If @repr @@ sh@ is a value type, then @'ReprDualOf' repr @@ sh@ is the
corresponding dual type at the same shape.
-}
type family ReprDualOf (a :: k ~> Type) :: k ~> Type

{- | Representation of tangent values.

If @repr @@ sh@ is a manifold point type, then @'ReprTang' repr @@ sh@ is the
corresponding tangent type at the same shape.
-}
type family ReprTang (a :: k ~> Type) :: k ~> Type

type family ReprGrad (a :: k ~> Type) :: k ~> Type where
    ReprGrad a = ReprDualOf (ReprTang a)

{- | Evidence that a representation is pointwise a 'VSpace'.

The dictionary also records the representation-level families for duals and
scalars, so generic record code can connect field-level 'DualOf' and 'Scalar'
associated types back to 'ReprDualOf' and 'ReprScalar'.
-}
class PointwiseVSpace field where
    vspaceDict ::
        forall sh.
        Sing sh ->
        Dict
            ( VSpace (field @@ sh)
            , DualOf (field @@ sh) ~ ReprDualOf field @@ sh
            , Scalar (field @@ sh) ~ ReprScalar field
            )

{- | A representation with enough evidence to make a generic structure a
'VSpace'.
-}
class
    ( SubtractiveRepr repr
    , SubtractiveRepr (ReprDualOf repr)
    , MultiplicativeActionRepr repr
    , MultiplicativeActionRepr (ReprDualOf repr)
    , PointwiseVSpace repr
    , ReprScalar (ReprDualOf repr) ~ ReprScalar repr
    , ReprDualOf (ReprDualOf repr) ~ repr
    , DualOf (ReprScalar repr) ~ ReprScalar repr
    , Scalar (ReprScalar repr) ~ ReprScalar repr
    , Ring (ReprScalar repr)
    ) =>
    VSpaceRepr repr

instance
    ( SubtractiveRepr repr
    , SubtractiveRepr (ReprDualOf repr)
    , MultiplicativeActionRepr repr
    , MultiplicativeActionRepr (ReprDualOf repr)
    , PointwiseVSpace repr
    , ReprScalar (ReprDualOf repr) ~ ReprScalar repr
    , ReprDualOf (ReprDualOf repr) ~ repr
    , DualOf (ReprScalar repr) ~ ReprScalar repr
    , Scalar (ReprScalar repr) ~ ReprScalar repr
    , Ring (ReprScalar repr)
    ) =>
    VSpaceRepr repr

-- | A representation whose every shape carries the chosen inner product.
class
    ( VSpaceRepr repr
    , AllConstrained InnerSpace repr
    ) =>
    InnerSpaceRepr repr

instance
    ( VSpaceRepr repr
    , AllConstrained InnerSpace repr
    ) =>
    InnerSpaceRepr repr

-- | Recover inner-space evidence and its representation-level equalities.
innerSpaceReprDict ::
    forall repr sh.
    (InnerSpaceRepr repr) =>
    Sing sh ->
    Dict
        ( InnerSpace (repr @@ sh)
        , DualOf (repr @@ sh) ~ ReprDualOf repr @@ sh
        , Scalar (repr @@ sh) ~ ReprScalar repr
        )
innerSpaceReprDict sh =
    case vspaceDict @repr sh of
        Dict -> case allConstrained @InnerSpace @repr sh of
            Dict -> Dict

{- | Evidence that each metric field acts on the covector field at the same
shape. Both representations may assign different types to different shapes.

The second representation contains covectors: 'Metric' maps each of these
to its dual vector type. Representation-level dual equalities can be recovered
separately through 'PointwiseVSpace'.
-}
class PointwiseMetric metric cov where
    metricDict ::
        forall sh.
        Sing sh ->
        Dict (Metric (metric @@ sh) (cov @@ sh))

{- | Evidence that a representation is pointwise a 'Manifold'.

The dictionary connects each field's 'Tang' associated type with the
representation-level 'ReprTang' family.
-}
class PointwiseManifold field where
    manifoldDict ::
        forall sh.
        Sing sh ->
        Dict
            ( Manifold (field @@ sh)
            , Tang (field @@ sh) ~ ReprTang field @@ sh
            )

manifoldReprDict ::
    forall leaf sh.
    (ManifoldRepr leaf) =>
    Sing sh ->
    Dict
        ( Manifold (leaf @@ sh)
        , Tang (leaf @@ sh) ~ ReprTang leaf @@ sh
        , VSpace (ReprTang leaf @@ sh)
        , DualOf (ReprTang leaf @@ sh) ~ ReprDualOf (ReprTang leaf) @@ sh
        , Scalar (ReprTang leaf @@ sh) ~ ReprScalar (ReprTang leaf)
        )
manifoldReprDict sh =
    case manifoldDict @leaf sh of
        Dict -> case vspaceDict @(ReprTang leaf) sh of
            Dict -> Dict

-- | A manifold representation with tangent and gradient builders.
type HasGradRepr repr =
    (ManifoldRepr repr, BuilderRepr (ReprTang repr), BuilderRepr (ReprGrad repr))

-- | Recover field-level gradient support and its representation equalities.
hasGradReprDict ::
    forall repr sh.
    (HasGradRepr repr) =>
    Sing sh ->
    Dict
        ( HasGrad (repr @@ sh)
        , Tang (repr @@ sh) ~ ReprTang repr @@ sh
        , Grad (repr @@ sh) ~ ReprGrad repr @@ sh
        , Scalar (ReprTang repr @@ sh) ~ ReprScalar (ReprTang repr)
        )
hasGradReprDict sh =
    case manifoldReprDict @repr sh of
        Dict -> case allConstrained @HasBuilder @(ReprTang repr) sh of
            Dict -> case allConstrained @HasBuilder @(ReprGrad repr) sh of
                Dict -> Dict

type ManifoldRepr repr =
    ( PointwiseManifold repr
    , VSpaceRepr (ReprTang repr)
    , ReprTang (ReprTang repr) ~ ReprTang repr
    , ReprTang (ReprDualOf (ReprTang repr)) ~ ReprDualOf (ReprTang repr)
    , Tang (ReprScalar (ReprTang repr)) ~ ReprScalar (ReprTang repr)
    )

type instance ReprScalar (ConstSym1 a) = Scalar a

type instance ReprTang (ConstSym1 a) = ConstSym1 (Tang a)
type instance ReprDualOf (ConstSym1 a) = ConstSym1 (DualOf a)

instance (c a) => AllConstrained c (ConstSym1 a) where
    allConstrained _ = Dict

instance (VSpace a) => PointwiseVSpace (ConstSym1 a) where
    vspaceDict _ = Dict

instance (Manifold a) => PointwiseManifold (ConstSym1 a) where
    manifoldDict _ = Dict

instance (Metric g a) => PointwiseMetric (ConstSym1 g) (ConstSym1 a) where
    metricDict _ = Dict
