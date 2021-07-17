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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.DVar
  ( -- * DVar
    DVar (..),

    -- * BVar
    BVar,
    var,
    constant,
    backprop,

    -- * Lift

    -- | Apply differentiable function to 'BVar'
    --liftFun1,
    --liftFun2,
    --liftFun3,
    {-
        -- * Easy lift
        easyLift1,
        easyLift2,
        easyLift3,
    -}
  )
where

import Data.AdditiveGroup (AdditiveGroup)
--GradBuilder,

--Scalar,

import Data.AffineSpace (AffineSpace ((.+^), (.-.)))
import qualified Data.AffineSpace as AffineSpace
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace ((*^)),
  )
import qualified Data.VectorSpace as VectorSpace
import Downhill.Grad (Dual (evalGrad), HasGrad (Diff, Grad, Scalar))
import Downhill.Linear.BackGrad
  ( BackGrad (..),
    castNode,
    realNode,
  )
import Downhill.Linear.Expr (BackFun, BasicVector, Expr (ExprSum, ExprVar), FullVector (identityBuilder), Term)
import qualified Downhill.Linear.Graph as Graph
import Prelude hiding (id, (.))

-- | Variable is a value paired with derivative. Derivative @dvarGrad@ is some kind of a linear
-- map @r -> a@ for some @r@. Type @d@ determines both @r@ and a way of encoding derivatives.
--
-- In case of @d ~ BackGrad r@, @dvarGrad@ stores computational graph of derivatives, enabling reverse mode
-- differentiantion. Choosing @d ~ Identity@ turns @DVar@ into dual number,
-- giving rise to simple forward mode differentiation.
data DVar r p = DVar
  { dvarValue :: p,
    dvarGrad :: BackGrad r (Grad p)
  }

instance (AdditiveGroup b, HasGrad b) => AdditiveGroup (DVar r b) where
  zeroV = DVar zeroV zeroV
  negateV (DVar y0 dy) = DVar (negateV y0) (negateV dy)
  DVar y0 dy ^-^ DVar z0 dz = DVar (y0 ^-^ z0) (dy ^-^ dz)
  DVar y0 dy ^+^ DVar z0 dz = DVar (y0 ^+^ z0) (dy ^+^ dz)

instance (Num b, HasGrad b, Scalar b ~ b) => Num (DVar r b) where
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

instance (Fractional b, HasGrad b, Scalar b ~ b) => Fractional (DVar r b) where
  fromRational x = DVar (fromRational x) zeroV
  recip (DVar x dx) = DVar (recip x) (df *^ dx)
    where
      df = negate (recip (sqr x))
  DVar x dx / DVar y dy = DVar (x / y) ((recip y *^ dx) ^-^ ((x / sqr y) *^ dy))

instance (Floating b, HasGrad b, Scalar b ~ b) => Floating (DVar r b) where
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
    HasGrad v,
    Diff v ~ v,
    FullVector (Scalar v),
    Grad (Scalar v) ~ Scalar v
  ) =>
  VectorSpace (DVar dr v)
  where
  type Scalar (DVar dr v) = DVar dr (Scalar v)
  DVar a (BackGrad da) *^ DVar v (BackGrad dv) = DVar (a *^ v) (castNode node)
    where
      node :: Expr BackFun dr (Grad v)
      node = ExprSum (term1 ++ term2)
        where
          term1 :: [Term BackFun dr (Grad v)]
          term1 = da (\v' -> identityBuilder (evalGrad @(Scalar v) @(Grad v) @(Diff v) v' v))
          term2 :: [Term BackFun dr (Grad v)]
          term2 = dv (\v' -> identityBuilder (a *^ v'))

instance
  ( AffineSpace p,
    HasGrad p,
    HasGrad (Diff p),
    Diff p ~ AffineSpace.Diff p,
    Grad (Diff p) ~ Grad p
  ) =>
  AffineSpace (DVar dr p)
  where
  type Diff (DVar dr p) = DVar dr (Diff p)
  DVar y0 dy .+^ DVar z0 dz = DVar (y0 .+^ z0) (dy ^+^ dz)
  DVar y0 dy .-. DVar z0 dz = DVar (y0 .-. z0) (dy ^-^ dz)

-- | 'DVar' specialized for reverse mode differentiation.
-- type BVar a p = DVar p (BackGrad a (Needle p))
type BVar = DVar

-- | A variable with derivative of zero.
constant :: forall r a. a -> BVar r a
constant x = DVar x (BackGrad (const [])) -- could be zeroV here, but that would require `HasDual a` constraint..

-- | A variable with identity derivative.
var :: a -> BVar (Grad a) a
var x = DVar x (realNode ExprVar)

-- | Compute gradient
backprop :: forall da v. (HasGrad v, BasicVector da, FullVector (Grad v)) => BVar da v -> Grad v -> da
backprop (DVar _y0 x) = Graph.backprop x
