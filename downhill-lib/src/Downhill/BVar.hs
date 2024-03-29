{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Downhill.BVar
  ( BVar (..),
    var,
    constant,
    backprop,

    -- * Pattern synonyms
    pattern T2,
    pattern T3,
  )
where

import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace (AffineSpace ((.+^), (.-.)))
import qualified Data.AffineSpace as AffineSpace
import Data.VectorSpace
  ( AdditiveGroup (..),
    InnerSpace ((<.>)),
    VectorSpace (Scalar, (*^)),
  )
import Downhill.Grad
  ( Dual (evalGrad),
    HasGrad,
    HasGradAffine,
    HilbertSpace (coriesz, riesz),
    MScalar,
    Manifold (Grad, Tang),
  )
import Downhill.Linear.BackGrad
  ( BackGrad (..),
    realNode,
  )
import qualified Downhill.Linear.Backprop as BP
import Downhill.Linear.Expr (BasicVector (..), Expr (ExprVar))
import Downhill.Linear.Lift (lift1_dense, lift2_dense)
import qualified Downhill.Linear.Prelude as Linear
import Prelude hiding (id, (.))

-- | Variable is a value paired with derivative.
data BVar r a = BVar
  { bvarValue :: a,
    bvarGrad :: BackGrad r (Grad a)
  }

instance (AdditiveGroup b, HasGrad b) => AdditiveGroup (BVar r b) where
  zeroV = BVar zeroV zeroV
  negateV (BVar y0 dy) = BVar (negateV y0) (negateV dy)
  BVar y0 dy ^-^ BVar z0 dz = BVar (y0 ^-^ z0) (dy ^-^ dz)
  BVar y0 dy ^+^ BVar z0 dz = BVar (y0 ^+^ z0) (dy ^+^ dz)

instance (Num b, HasGrad b, MScalar b ~ b) => Num (BVar r b) where
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

instance (Fractional b, HasGrad b, MScalar b ~ b) => Fractional (BVar r b) where
  fromRational x = BVar (fromRational x) zeroV
  recip (BVar x dx) = BVar (recip x) (df *^ dx)
    where
      df = negate (recip (sqr x))
  BVar x dx / BVar y dy = BVar (x / y) ((recip y *^ dx) ^-^ ((x / sqr y) *^ dy))

instance (Floating b, HasGrad b, MScalar b ~ b) => Floating (BVar r b) where
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
    HasGrad v,
    Tang v ~ v,
    HasGrad (MScalar v),
    Grad (Scalar v) ~ Scalar v
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

instance (HasGrad p, HasGradAffine p) => AffineSpace (BVar r p) where
  type Diff (BVar r p) = BVar r (Tang p)
  BVar y0 dy .+^ BVar z0 dz = BVar (y0 .+^ z0) (dy ^+^ dz)
  BVar y0 dy .-. BVar z0 dz = BVar (y0 .-. z0) (dy ^-^ dz)

-- maybe move all those equality constraints to Dual class?
instance
  ( HasGrad (Scalar v),
    HasGrad v,
    HasGrad dv,
    Dual v dv,
    Grad dv ~ v,
    Grad v ~ dv,
    Tang v ~ v,
    Tang dv ~ dv,
    Grad (Scalar dv) ~ Scalar dv
  ) =>
  Dual (BVar r v) (BVar r dv)
  where
  evalGrad (BVar dv d_dv) (BVar v d_v) = BVar (evalGrad dv v) (lift2_dense (*^ v) (*^ dv) d_dv d_v)

instance
  ( HasGrad (MScalar p),
    HasGrad (Tang p),
    HasGrad (Grad p),
    Grad (Grad p) ~ Tang p,
    Tang (Grad p) ~ Grad p,
    Tang (Tang p) ~ Tang p,
    Grad (Tang p) ~ Grad p,
    Grad (MScalar p) ~ MScalar p,
    Scalar (Grad p) ~ Scalar (Tang p),
    Manifold p
  ) =>
  Manifold (BVar r p)
  where
  type Tang (BVar r p) = BVar r (Tang p)
  type Grad (BVar r p) = BVar r (Grad p)

instance
  ( HilbertSpace v dv,
    HasGrad (Scalar v),
    HasGrad v,
    HasGrad dv,
    Grad dv ~ v,
    Grad v ~ dv,
    Tang v ~ v,
    Tang dv ~ dv,
    Grad (Scalar dv) ~ Scalar dv
  ) =>
  HilbertSpace (BVar r v) (BVar r dv)
  where
  riesz (BVar v dv) = BVar (riesz v) (lift1_dense riesz dv)
  coriesz (BVar v dv) = BVar (coriesz v) (lift1_dense coriesz dv)

instance
  ( VectorSpace v,
    HasGrad v,
    Tang v ~ v,
    HilbertSpace (Tang v) (Grad v),
    BasicVector (MScalar v),
    Grad (MScalar v) ~ MScalar v,
    InnerSpace v,
    HasGrad (MScalar v)
  ) =>
  InnerSpace (BVar r v)
  where
  BVar u du <.> BVar v dv = BVar (u <.> v) (lift2_dense bpU bpV du dv)
    where
      bpU :: MScalar v -> Grad v
      bpU dz = dz *^ riesz v
      bpV :: MScalar v -> Grad v
      bpV dz = dz *^ riesz u

-- | A variable with derivative of zero.
constant :: forall r a. (BasicVector (Grad a), AdditiveGroup (Grad a)) => a -> BVar r a
constant x = BVar x zeroV

-- | A variable with identity derivative.
var :: a -> BVar (Grad a) a
var x = BVar x (realNode ExprVar)

-- | Reverse mode differentiation.
backprop :: forall r a. (HasGrad a, BasicVector r) => BVar r a -> Grad a -> r
backprop (BVar _y0 x) = BP.backprop x

splitPair :: (BasicVector (Grad a), BasicVector (Grad b)) => BVar r (a, b) -> (BVar r a, BVar r b)
splitPair (BVar (a, b) (Linear.T2 da db)) = (BVar a da, BVar b db)

pattern T2 :: forall r a b. (BasicVector (Grad a), BasicVector (Grad b)) => BVar r a -> BVar r b -> BVar r (a, b)
pattern T2 a b <-
  (splitPair -> (a, b))
  where
    T2 (BVar a da) (BVar b db) = BVar (a, b) (Linear.T2 da db)

splitTriple :: (BasicVector (Grad a), BasicVector (Grad b), BasicVector (Grad c)) => BVar r (a, b, c) -> (BVar r a, BVar r b, BVar r c)
splitTriple (BVar (a, b, c) (Linear.T3 da db dc)) = (BVar a da, BVar b db, BVar c dc)

pattern T3 ::
  forall r a b c.
  (BasicVector (Grad a), BasicVector (Grad b), BasicVector (Grad c)) =>
  BVar r a ->
  BVar r b ->
  BVar r c ->
  BVar r (a, b, c)
pattern T3 a b c <-
  (splitTriple -> (a, b, c))
  where
    T3 (BVar a da) (BVar b db) (BVar c dc) = BVar (a, b, c) (Linear.T3 da db dc)
