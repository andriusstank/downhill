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

module Downhill.BVar
  ( BVar (..),
    var,
    constant,
    backprop, backprop'
  )
where

import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace (AffineSpace ((.+^), (.-.)))
import qualified Data.AffineSpace as AffineSpace
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace ((*^)),
  )
import qualified Data.VectorSpace as VectorSpace
import Downhill.Grad (Dual (evalGrad), HasGrad (Tang, Grad, MScalar), GradBuilder,
 HasGradAffine, HasFullGrad)
import Downhill.Linear.BackGrad
  ( BackGrad (..),
    realNode,
  )
import Downhill.Linear.Expr (Expr (ExprVar), FullVector, BasicVector)
import qualified Downhill.Linear.Backprop as BP
import Downhill.Linear.Lift (lift2_dense)
import Prelude hiding (id, (.))

-- | Variable is a value paired with derivative.
data BVar r p = BVar
  { bvarValue :: p,
    bvarGrad :: BackGrad r (Grad p)
  }

instance (AdditiveGroup b, HasFullGrad b) => AdditiveGroup (BVar r b) where
  zeroV = BVar zeroV zeroV
  negateV (BVar y0 dy) = BVar (negateV y0) (negateV dy)
  BVar y0 dy ^-^ BVar z0 dz = BVar (y0 ^-^ z0) (dy ^-^ dz)
  BVar y0 dy ^+^ BVar z0 dz = BVar (y0 ^+^ z0) (dy ^+^ dz)

instance (Num b, HasFullGrad b, MScalar b ~ b) => Num (BVar r b) where
  (BVar f0 df) + (BVar g0 dg) = BVar (f0 + g0) (df ^+^ dg)
  (BVar f0 df) - (BVar g0 dg) = BVar (f0 - g0) (df ^-^ dg)
  (BVar f0 df) * (BVar g0 dg) = BVar (f0 * g0) (f0 *^ dg ^+^ g0 *^ df)
  negate (BVar f0 df) = BVar (negate f0) (negateV df)
  abs (BVar f0 df) = BVar (abs f0) (signum f0 *^ df) -- TODO: ineffiency: multiplication by 1
  signum (BVar f0 _) = BVar (signum f0) zeroV
  fromInteger x = BVar (fromInteger x) zeroV

sqr :: Num a => a -> a
sqr x = x * x

rsqrt :: Floating a => a -> a
rsqrt x = recip (sqrt x)

instance (Fractional b, HasFullGrad b, MScalar b ~ b) => Fractional (BVar r b) where
  fromRational x = BVar (fromRational x) zeroV
  recip (BVar x dx) = BVar (recip x) (df *^ dx)
    where
      df = negate (recip (sqr x))
  BVar x dx / BVar y dy = BVar (x / y) ((recip y *^ dx) ^-^ ((x / sqr y) *^ dy))

instance (Floating b, HasFullGrad b, MScalar b ~ b) => Floating (BVar r b) where
  pi = BVar pi zeroV
  exp (BVar x dx) = BVar (exp x) (exp x *^ dx)
  log (BVar x dx) = BVar (log x) (recip x *^ dx)
  sin (BVar x dx) = BVar (sin x) (cos x *^ dx)
  cos (BVar x dx) = BVar (cos x) (negate (sin x) *^ dx)
  asin (BVar x dx) = BVar (asin x) (rsqrt (1 - sqr x) *^ dx)
  acos (BVar x dx) = BVar (acos x) (negate (rsqrt (1 - sqr x)) *^ dx)
  atan (BVar x dx) = BVar (atan x) (recip (1 + sqr x) *^ dx)
  sinh (BVar x dx) = BVar (sinh x) (cosh x *^ dx)
  cosh (BVar x dx) = BVar (cosh x) (sinh x *^ dx)
  asinh (BVar x dx) = BVar (asinh x) (rsqrt (1 + sqr x) *^ dx)
  acosh (BVar x dx) = BVar (acosh x) (rsqrt (sqr x - 1) *^ dx)
  atanh (BVar x dx) = BVar (atanh x) (recip (1 - sqr x) *^ dx)

instance
  ( VectorSpace v,
    HasFullGrad v,
    Tang v ~ v,
    FullVector (MScalar v),
    Grad (MScalar v) ~ MScalar v
  ) =>
  VectorSpace (BVar r v)
  where
  type Scalar (BVar r v) = BVar r (MScalar v)
  BVar a da *^ BVar v dv = BVar (a *^ v) (lift2_dense bpA bpV da dv)
    where
      bpA :: Grad v -> MScalar v
      bpA dz = evalGrad dz v
      bpV :: Grad v -> Grad v
      bpV dz = a *^ dz

instance (HasFullGrad p, HasGradAffine p) => AffineSpace (BVar r p) where
  type Diff (BVar r p) = BVar r (Tang p)
  BVar y0 dy .+^ BVar z0 dz = BVar (y0 .+^ z0) (dy ^+^ dz)
  BVar y0 dy .-. BVar z0 dz = BVar (y0 .-. z0) (dy ^-^ dz)

-- | A variable with derivative of zero.
constant :: forall r a. a -> BVar r a
constant x = BVar x (BackGrad (const [])) -- could be zeroV here, but that would require `HasDual a` constraint..

-- | A variable with identity derivative.
var :: a -> BVar (Grad a) a
var x = BVar x (realNode ExprVar)

-- | Backpropagation.
backprop :: forall a p. (HasGrad p, BasicVector a) => BVar a p -> GradBuilder p -> a
backprop (BVar _y0 x) = BP.backprop x

-- | Like @backprop@, but takes @Grad p@  instead of @GradBuilder p@.
backprop' :: forall a p. (HasGrad p, FullVector (Grad p), BasicVector a) => BVar a p -> Grad p -> a
backprop' (BVar _y0 x) = BP.backprop' x
