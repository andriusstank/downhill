{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module DownhillTest.GD ()
where

import Downhill.BVar (BVar, DFun)
import Downhill.BVar qualified as BVar
import Downhill.Linear.Builder (HasBuilder, Dense)
import Downhill.Linear.Fun (DualOf, LinearFun (..))
import Downhill.Manifold (Manifold(..), Tang, IsScalar)
import Downhill.VSpace (InnerSpace, VSpace (..), sqrNorm)
import NumHask.Prelude
import Downhill.Linear.Builder (Dense(..))
import Data.Constraint (Dict(..))

loss1 :: (InnerSpace v, FromRational (Scalar v)) => v -> Scalar v
loss1 x = 0.5 * sqrNorm x

bvarLoss1 ::
    forall r v.
    ( InnerSpace v
    , VSpace (DualOf v)
    , Manifold v
    , Manifold (Scalar v)
    , Tang v ~ v
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    , Fractional (Scalar v)
    , FromRational (Scalar v)
    ) =>
    BVar r v -> BVar r (Scalar v)
bvarLoss1 = loss1

dloss1 :: forall v.
    ( InnerSpace v, Tang v ~ v
    , Tang (Scalar v) ~ Scalar v
    , Tang (DualOf v) ~ DualOf v
    , Manifold v
    , Manifold (Scalar v)
    , VSpace (DualOf v)
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    , Fractional (Scalar v)
    , FromRational (Scalar v)
    ) =>
    BVar.DFun v (Scalar v) (BVar.Graph v (Scalar v))
dloss1 = BVar.ad bvarLoss1

data Vec2 a = Vec2 a a
    deriving Generic
    deriving (Additive, Subtractive, Multiplicative, MultiplicativeAction) via Generically (Vec2 a)
    deriving HasBuilder via (Dense (Vec2 a))

deriving via Generically (Vec2 a) instance (Scalar a ~ a, Tang a ~ a, DualOf a ~ a, Manifold a) => Manifold (Vec2 a)

data Cov2 a = Cov2 a a
    deriving Generic
    deriving (Additive, Subtractive, Multiplicative, MultiplicativeAction) via Generically (Cov2 a)
    deriving HasBuilder via (Dense (Cov2 a))

instance (IsScalar a, Multiplicative a) => VSpace (Vec2 a) where
    Cov2 dx dy |*| Vec2 x y = dx*x + dy*y

type instance Tang (Vec2 a) = Vec2 a
type instance Tang (Cov2 a) = Cov2 a
type instance DualOf (Vec2 a) = Cov2 a
type instance DualOf (Cov2 a) = Vec2 a

{-
instance (Scalar a ~ a, DualOf a ~ a, Tang a ~ a, IsScalar a) => Manifold (Vec2 a) where
    (.+~^) = (+)
    (.-~^) = (-)
-}
