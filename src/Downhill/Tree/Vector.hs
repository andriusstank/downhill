{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A reusable leaf universe for trees of scalars and vectors.

'ParameterLeaf' is parameterized by a vector-space type @v@. Scalar leaves
contain @Scalar v@, while vector leaves contain @v@ itself. Applying the
representation to tangent, dual, or 'BVar' vector types therefore transforms
both kinds of leaves consistently.
-}
module Downhill.Tree.Vector (
    ParameterShape (..),
    SParameterShape (..),
    ParameterLeaf,
)
where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, Sing, SingI (sing), type (~>))
import Downhill.BVar (BVar)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold, Tang)
import Downhill.Tree.BVar (BVarRepr (..))
import Downhill.Tree.Representation (
    AllConstrained (allConstrained),
    PointwiseManifold (manifoldDict),
    PointwiseVSpace (vspaceDict),
    ReprDualOf,
    ReprScalar,
    ReprTang,
 )
import Downhill.VSpace (VSpace)
import NumHask.Prelude

-- | Shapes available in the parameter leaf universe.
data ParameterShape
    = ScalarParameter
    | VectorParameter

-- | Singleton witnesses for 'ParameterShape'.
data SParameterShape (sh :: ParameterShape) where
    SScalarParameter :: SParameterShape 'ScalarParameter
    SVectorParameter :: SParameterShape 'VectorParameter

type instance Sing = SParameterShape

instance SingI 'ScalarParameter where
    sing = SScalarParameter

instance SingI 'VectorParameter where
    sing = SVectorParameter

-- | Represent scalar leaves by @Scalar v@ and vector leaves by @v@.
data ParameterLeaf (v :: Type) :: ParameterShape ~> Type

type instance Apply (ParameterLeaf v) 'ScalarParameter = Scalar v
type instance Apply (ParameterLeaf v) 'VectorParameter = v

type instance ReprScalar (ParameterLeaf v) = Scalar v
type instance ReprTang (ParameterLeaf v) = ParameterLeaf (Tang v)
type instance ReprDualOf (ParameterLeaf v) = ParameterLeaf (DualOf v)

instance (c (Scalar v), c v) => AllConstrained c (ParameterLeaf v) where
    allConstrained SScalarParameter = Dict
    allConstrained SVectorParameter = Dict

instance (VSpace (Scalar v), VSpace v) => PointwiseVSpace (ParameterLeaf v) where
    vspaceDict SScalarParameter = Dict
    vspaceDict SVectorParameter = Dict

instance
    ( Manifold v
    , Manifold (Scalar v)
    , Tang (Scalar v) ~ Scalar (Tang v)
    ) =>
    PointwiseManifold (ParameterLeaf v)
    where
    manifoldDict SScalarParameter = Dict
    manifoldDict SVectorParameter = Dict

instance BVarRepr (ParameterLeaf v) where
    type ReprBVar r (ParameterLeaf v) = ParameterLeaf (BVar r v)
    splitBVarField SScalarParameter = id
    splitBVarField SVectorParameter = id
    joinBVarField SScalarParameter = id
    joinBVarField SVectorParameter = id
