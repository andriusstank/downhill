{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.AD (
    AD (..),
    Bigraph,
    ad,
    adDense
)
where

import Downhill.Internal.Graph.Bigraph (Bigraph, DenseBigraph (DenseBigraph), buildBigraph)
import Downhill.Linear.Builder (Dense, HasBuilder (..))
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum), Term (Term), VecPair (VecPair), idBiFun)
import Downhill.Linear.Fun (DualOf, IdFun (IdFun), LinearFun ((<*|), (|*>)))
import NumHask.Prelude

newtype AD (a :: Type) (v :: Type)
    = AD
        ( forall x.
          BiFun ('VecPair v (DualOf v)) x ->
          Term ('VecPair a (DualOf a)) x
        )

realNode :: (dv ~ DualOf v) => Expr ('VecPair a (DualOf a)) ('VecPair v dv) -> AD a v
realNode x = AD (\(BiFun f g) -> Term (BiFun f g) x)

denseTerm ::
    ( LinearFun f u v
    , HasBuilder v
    , HasBuilder (DualOf u)
    ) =>
    f -> AD r u -> Term ('VecPair r (DualOf r)) ('VecPair v (DualOf v))
denseTerm f (AD x) = x $ BiFun (identityBuilder . (f <*|)) (identityBuilder . (|*> f))

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

ad ::
    forall a z.
    ( HasBuilder a
    , HasBuilder z
    , HasBuilder (DualOf z)
    , HasBuilder (DualOf a)
    ) =>
    AD a z -> Bigraph a z
ad (AD f) = buildBigraph [f idBiFun]

adDense ::
    (Additive a, Additive z, Additive (DualOf z), Additive (DualOf a)) =>
    AD (Dense a) (Dense z) -> DenseBigraph a z
adDense = DenseBigraph . ad