{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.AD (
    AD (..),
    Graph,
    ad,
    adDense,
    identityAD,
    dense1, dense2
)
where

import Downhill.Internal.Graph.Graph (Graph, DenseBigraph (DenseBigraph), buildBigraph)
import Downhill.Linear.Builder (Dense, HasBuilder (..))
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum, ExprVar), Term (Term), VecPair (VecPair), idBiFun)
import Downhill.Linear.Fun (DualOf, IdFun (IdFun), LinearFun ((<*|), (|*>)), NegateFun (NegateFun))
import NumHask.Prelude

newtype AD (a :: Type) (v :: Type)
    = AD
        ( forall x.
          BiFun ('VecPair v (DualOf v)) x ->
          Term ('VecPair a (DualOf a)) x
        )

identityAD :: AD a a
identityAD = realNode ExprVar

realNode :: (dv ~ DualOf v) => Expr ('VecPair a (DualOf a)) ('VecPair v dv) -> AD a v
realNode x = AD (\(BiFun f g) -> Term (BiFun f g) x)

denseTerm ::
    ( LinearFun f u v
    , HasBuilder v
    , HasBuilder (DualOf u)
    ) =>
    f -> AD r u -> Term ('VecPair r (DualOf r)) ('VecPair v (DualOf v))
denseTerm f (AD x) = x $ BiFun (identityBuilder . (f <*|)) (identityBuilder . (|*> f))

dense1 ::
    ( LinearFun f u v
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (DualOf u)
    ) =>
    f ->
    AD r u ->
    AD r v
dense1 f1 x =
    realNode
        ( ExprSum
            [ denseTerm f1 x
            ]
        )

dense2 ::
    ( LinearFun f1 u1 v
    , LinearFun f2 u2 v
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (DualOf u1)
    , HasBuilder (DualOf u2)
    ) =>
    f1 ->
    f2 ->
    AD r u1 ->
    AD r u2 ->
    AD r v
dense2 f1 f2 x y =
    realNode
        ( ExprSum
            [ denseTerm f1 x
            , denseTerm f2 y
            ]
        )

instance (HasBuilder v, HasBuilder (DualOf v)) => Additive (AD r v) where
    zero = realNode (ExprSum [])
    (+) = dense2 IdFun IdFun

instance (HasBuilder v, HasBuilder (DualOf v), Subtractive v, Subtractive (DualOf v)) => Subtractive (AD r v) where
    negate = dense1 NegateFun
    (-) = dense2 IdFun NegateFun

ad ::
    forall a z.
    ( HasBuilder a
    , HasBuilder z
    , HasBuilder (DualOf z)
    , HasBuilder (DualOf a)
    ) =>
    AD a z -> Graph a z
ad (AD f) = buildBigraph [f idBiFun]

adDense ::
    (Additive a, Additive z, Additive (DualOf z), Additive (DualOf a)) =>
    AD (Dense a) (Dense z) -> DenseBigraph a z
adDense = DenseBigraph . ad