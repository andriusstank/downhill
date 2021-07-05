{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Downhill.DVarH
  ( DVar (..),
    BVarH,
  )
where

import Data.AdditiveGroup (AdditiveGroup ((^+^), (^-^)))
import Data.Kind (Type)
import Data.Singletons (Apply, TyCon1, type (@@), type (~>))
import Data.VectorSpace (AdditiveGroup (negateV, zeroV), VectorSpace (Scalar, (*^)))
import Downhill.Linear.BackGrad (BackGrad (BackGrad), HasDual (DualOf, evalGrad), castNode, realNode)
import Downhill.Linear.Expr (BackFun (BackFun), Expr (ExprSum, ExprVar), FullVector (identityBuilder), Term (Term), BasicVector (VecBuilder))
import Math.Manifold.Core.PseudoAffine (Semimanifold (Needle))
import qualified Downhill.Linear.Graph as Graph
import Downhill.Linear.Lift (LinFun1(LinFun1))

import qualified Downhill.Linear.Lift as Linear

data DVar d a = DVar
  { dvarValue :: a,
    dvarGrad :: (d @@ a)
  }

instance (AdditiveGroup a, AdditiveGroup (d @@ a)) => AdditiveGroup (DVar d a) where
  zeroV = DVar zeroV zeroV
  negateV (DVar y0 dy) = DVar (negateV y0) (negateV dy)
  DVar y0 dy ^-^ DVar z0 dz = DVar (y0 ^-^ z0) (dy ^-^ dz)
  DVar y0 dy ^+^ DVar z0 dz = DVar (y0 ^+^ z0) (dy ^+^ dz)

instance (Num a, VectorSpace (d @@ a), a ~ Scalar (d @@ a)) => Num (DVar d a) where
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

instance (Fractional a, VectorSpace (d @@ a), a ~ Scalar (d @@ a)) => Fractional (DVar d a) where
  fromRational x = DVar (fromRational x) zeroV
  recip (DVar x dx) = DVar (recip x) (df *^ dx)
    where
      df = negate (recip (sqr x))
  DVar x dx / DVar y dy = DVar (x / y) ((recip y *^ dx) ^-^ ((x / sqr y) *^ dy))

instance (Floating a, VectorSpace (d @@ a), a ~ Scalar (d @@ a)) => Floating (DVar d a) where
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

--type BackGradFam a = TyCon1

instance
  ( VectorSpace v,
    VectorSpace (DualOf v),
    FullVector (DualOf (Scalar v)),
    Scalar (DualOf v) ~ Scalar v,
    HasDual v
  ) =>
  VectorSpace (DVar (TyCon1 (BackGrad a)) v)
  where
  type Scalar (DVar (TyCon1 (BackGrad a)) v) = DVar (TyCon1 (BackGrad a)) (Scalar v)
  DVar a (BackGrad da) *^ DVar v (BackGrad dv) = DVar (a *^ v) (castNode node)
    where
      node :: Expr BackFun (DualOf a) (DualOf v)
      node = ExprSum (term1 ++ term2)
        where
          term1 :: [Term BackFun (DualOf a) (DualOf v)]
          term1 = da (\v' -> identityBuilder (evalGrad v' v))
          term2 :: [Term BackFun (DualOf a) (DualOf v)]
          term2 = dv (\v' -> identityBuilder (a *^ v'))

data BackGradSym a :: Type ~> Type

type instance Apply (BackGradSym a) p = BackGrad (Needle a) (Needle p)

--type family BackGrad a p where
--  BackGradH a p = BackGrad a (Needle p)

type BVarH a p = DVar (BackGradSym a) p

type HasGrad p = HasDual (Needle p)

type GradOf p = DualOf (Needle p)

type GradBuilder v = VecBuilder (DualOf (Needle v))

-- | A variable with derivative of zero.
constant :: forall r a. a -> BVarH r a
constant x = DVar x (BackGrad (const [])) -- could be zeroV here, but that would require `HasDual a` constraint..

-- | A variable with identity derivative.
var :: a -> BVarH a a
var x = DVar x (realNode ExprVar)

-- | Compute gradient
backprop :: forall a v. (HasGrad v, HasGrad a) => BVarH a v -> GradOf v -> GradOf a
backprop (DVar _y0 x) = Graph.backprop x

liftFun1 ::
  forall r a b.
  (a -> (b, LinFun1 (Needle a) (Needle b))) ->
  BVarH r a ->
  BVarH r b
liftFun1 dfun (DVar a0 da) = DVar z0 (Linear.lift1 fa da)
  where
    (z0, fa) = dfun a0
