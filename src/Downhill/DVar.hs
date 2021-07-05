{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.DVar
  ( -- * DVar
    DVar (..),

    -- * BVar
    BVar,
    HasGrad,
    GradOf,
    var,
    constant,
    backprop,

    -- * Lift

    -- | Apply differentiable function to 'BVar'
    liftFun1,
    liftFun2,
    liftFun3,

    -- * Easy lift
    easyLift1,
    easyLift2,
    easyLift3,
  )
where

import Data.AdditiveGroup (AdditiveGroup)
import Data.VectorSpace
  ( AdditiveGroup (..),
    Scalar,
    VectorSpace (..),
  )
import Downhill.Linear.BackGrad
  ( BackGrad (..),
    --GradBuilder,
    HasDual (..),
    castNode,
    realNode,
  )
import Downhill.Linear.Expr (BackFun, BasicVector (VecBuilder), Expr (ExprSum, ExprVar), FullVector (identityBuilder), Term)
import qualified Downhill.Linear.Graph as Graph
import Downhill.Linear.Lift (LinFun1, LinFun2, LinFun3)
import qualified Downhill.Linear.Lift as Easy
import qualified Downhill.Linear.Lift as Lift
import Math.Manifold.Core.PseudoAffine (Semimanifold (Needle))
import Prelude hiding (id, (.))

-- | Variable is a value paired with derivative. Derivative @dvarGrad@ is some kind of a linear
-- map @r -> a@ for some @r@. Type @d@ determines both @r@ and a way of encoding derivatives.
--
-- In case of @d ~ BackGrad r@, @dvarGrad@ stores computational graph of derivatives, enabling reverse mode
-- differentiantion. Choosing @d ~ Identity@ turns @DVar@ into dual number,
-- giving rise to simple forward mode differentiation.
data DVar r p = DVar
  { dvarValue :: p,
    dvarGrad :: BackGrad r (Needle p)
  }

instance (AdditiveGroup b, HasGrad b) => AdditiveGroup (DVar r b) where
  zeroV = DVar zeroV zeroV
  negateV (DVar y0 dy) = DVar (negateV y0) (negateV dy)
  DVar y0 dy ^-^ DVar z0 dz = DVar (y0 ^-^ z0) (dy ^-^ dz)
  DVar y0 dy ^+^ DVar z0 dz = DVar (y0 ^+^ z0) (dy ^+^ dz)

instance (Num b, HasGrad b, Needle b ~ b, Scalar b ~ b) => Num (DVar r b) where
  (DVar f0 df) + (DVar g0 dg) = DVar (f0 + g0) (df ^+^ dg)
  (DVar f0 df) - (DVar g0 dg) = DVar (f0 - g0) (df ^-^ dg)
  (DVar f0 df) * (DVar g0 dg) = DVar (f0 * g0) (f0 *^ dg ^+^ g0 *^ df)
  negate (DVar f0 df) = DVar (negate f0) (negateV df)
  abs (DVar f0 df) = DVar (abs f0) (signum f0 *^ df) -- TODO: ineffiency: multiplication by 1
  signum (DVar f0 _) = DVar (signum f0) zeroV
  fromInteger x = DVar (fromInteger x) zeroV

sqr :: Num a => a -> a
sqr x = x * x

rsqrt :: Floating a => a -> a
rsqrt x = recip (sqrt x)

instance (Fractional b, HasGrad b, Needle b ~ b, Scalar b ~ b) => Fractional (DVar r b) where
  fromRational x = DVar (fromRational x) zeroV
  recip (DVar x dx) = DVar (recip x) (df *^ dx)
    where
      df = negate (recip (sqr x))
  DVar x dx / DVar y dy = DVar (x / y) ((recip y *^ dx) ^-^ ((x / sqr y) *^ dy))

instance (Floating b, HasGrad b, Needle b ~ b, Scalar b ~ b) => Floating (DVar r b) where
  pi = DVar pi zeroV
  exp (DVar x dx) = DVar (exp x) (exp x *^ dx)
  log (DVar x dx) = DVar (log x) (recip x *^ dx)
  sin (DVar x dx) = DVar (sin x) (cos x *^ dx)
  cos (DVar x dx) = DVar (cos x) (negate (sin x) *^ dx)
  asin (DVar x dx) = DVar (asin x) (rsqrt (1 - sqr x) *^ dx)
  acos (DVar x dx) = DVar (acos x) (negate (rsqrt (1 - sqr x)) *^ dx)
  atan (DVar x dx) = DVar (atan x) (recip (1 + sqr x) *^ dx)
  sinh (DVar x dx) = DVar (sinh x) (cosh x *^ dx)
  cosh (DVar x dx) = DVar (cosh x) (sinh x *^ dx)
  asinh (DVar x dx) = DVar (asinh x) (rsqrt (1 + sqr x) *^ dx)
  acosh (DVar x dx) = DVar (acosh x) (rsqrt (sqr x - 1) *^ dx)
  atanh (DVar x dx) = DVar (atanh x) (recip (1 - sqr x) *^ dx)

instance
  ( VectorSpace v,
    VectorSpace (DualOf v),
    HasDual v,
    Needle v ~ v,
    Needle (Scalar v) ~ Scalar v,
    FullVector (DualOf (Scalar v)),
    BasicVector (DualOf v)
  ) =>
  VectorSpace (DVar r v)
  where
  type Scalar (DVar r v) = DVar r (Scalar v)
  DVar a (BackGrad da) *^ DVar v (BackGrad dv) = DVar (a *^ v) (castNode node)
    where
      node :: Expr BackFun (DualOf r) (DualOf v)
      node = ExprSum (term1 ++ term2)
        where
          term1 :: [Term BackFun (DualOf r) (DualOf v)]
          term1 = da (\v' -> identityBuilder (evalGrad v' v))
          term2 :: [Term BackFun (DualOf r) (DualOf v)]
          term2 = dv (\v' -> identityBuilder (a *^ v'))

-- | 'DVar' specialized for reverse mode differentiation.
--type BVar a p = DVar p (BackGrad a (Needle p))
type BVar = DVar

type HasGrad p = HasDual (Needle p)

type GradOf p = DualOf (Needle p)

type GradBuilder v = VecBuilder (DualOf (Needle v))

-- | A variable with derivative of zero.
constant :: forall r a. a -> BVar r a
constant x = DVar x (BackGrad (const [])) -- could be zeroV here, but that would require `HasDual a` constraint..

-- | A variable with identity derivative.
var :: a -> BVar (Needle a) a
var x = DVar x (realNode ExprVar)

-- | Compute gradient
backprop :: forall a v. (HasGrad v, BasicVector (DualOf a)) => BVar a v -> GradOf v -> DualOf a
backprop (DVar _y0 x) = Graph.backprop x

liftFun1 ::
  forall r a b.
  (a -> (b, LinFun1 (Needle a) (Needle b))) ->
  BVar r a ->
  BVar r b
liftFun1 dfun (DVar a0 da) = DVar z0 (Lift.lift1 fa da)
  where
    (z0, fa) = dfun a0

liftFun2 ::
  forall x r a b z.
  (BasicVector x, VecBuilder x ~ GradBuilder z) =>
  (a -> b -> (z, LinFun2 (Needle a) (Needle b) (Needle z))) ->
  BVar r a ->
  BVar r b ->
  BVar r z
liftFun2 dfun (DVar a0 da) (DVar b0 db) = DVar z0 (Lift.lift2 f2 da db)
  where
    (z0, f2) = dfun a0 b0

liftFun3 ::
  forall r a b c z.
  () =>
  (a -> b -> c -> (z, LinFun3 (Needle a) (Needle b) (Needle c) (Needle z))) ->
  BVar r a ->
  BVar r b ->
  BVar r c ->
  BVar r z
liftFun3 dfun (DVar a0 da) (DVar b0 db) (DVar c0 dc) = DVar z0 (Lift.lift3 f3 da db dc)
  where
    (z0, f3) = dfun a0 b0 c0

easyLift1 ::
  BasicVector (DualOf (Needle z)) =>
  (a -> (z, DualOf (Needle z) -> GradBuilder a)) ->
  BVar r a ->
  BVar r z
easyLift1 f (DVar a da) = DVar z (Easy.easyLift1 (Easy.EasyFun1 df) da)
  where
    (z, df) = f a

easyLift2 ::
  BasicVector (DualOf (Needle z)) =>
  (a -> b -> (z, DualOf (Needle z) -> (GradBuilder a, GradBuilder b))) ->
  BVar r a ->
  BVar r b ->
  BVar r z
easyLift2 f (DVar a da) (DVar b db) = DVar z (Easy.easyLift2 (Easy.EasyFun2 df) da db)
  where
    (z, df) = f a b

easyLift3 ::
  BasicVector (DualOf (Needle z)) =>
  (a -> b -> c -> (z, DualOf (Needle z) -> (GradBuilder a, GradBuilder b, GradBuilder c))) ->
  BVar r a ->
  BVar r b ->
  BVar r c ->
  BVar r z
easyLift3 f (DVar a da) (DVar b db) (DVar c dc) = DVar z (Easy.easyLift3 (Easy.EasyFun3 df) da db dc)
  where
    (z, df) = f a b c
