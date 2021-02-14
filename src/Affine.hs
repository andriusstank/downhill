{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Affine where

import Data.AffineSpace (AffineSpace((.-.), Diff))
import Tensor (TensorProduct(..))
import Data.AdditiveGroup (AdditiveGroup((^+^)))
import Data.VectorSpace (AdditiveGroup((^-^), negateV, zeroV), VectorSpace(Scalar, (*^)))

data AffineFunc b dv = AffineFunc b dv

evalAffineFunc :: (AdditiveGroup b, TensorProduct'' dv v b) => AffineFunc b dv -> v -> b
evalAffineFunc (AffineFunc y0 dydx) x = y0 ^+^ (dydx ⊗ x)

instance (AdditiveGroup b, AdditiveGroup dv) => AdditiveGroup (AffineFunc b dv) where
    zeroV = AffineFunc zeroV zeroV
    negateV (AffineFunc y0 dy) = AffineFunc (negateV y0) (negateV dy)
    AffineFunc y0 dy ^-^ AffineFunc z0 dz = AffineFunc (y0 ^-^ z0) (dy ^-^ dz)
    AffineFunc y0 dy ^+^ AffineFunc z0 dz = AffineFunc (y0 ^+^ z0) (dy ^+^ dz)

instance (VectorSpace b, VectorSpace dv, Scalar b ~ Scalar dv) => VectorSpace (AffineFunc b dv) where
    type Scalar (AffineFunc b dv) = Scalar b
    a *^ AffineFunc y0 dy = AffineFunc (a *^ y0) (a *^ dy)

instance (Num b, VectorSpace dv, b ~ Scalar dv) => Num (AffineFunc b dv) where
    (AffineFunc f0 df) + (AffineFunc g0 dg) = AffineFunc (f0+g0) (df ^+^ dg)
    (AffineFunc f0 df) - (AffineFunc g0 dg) = AffineFunc (f0-g0) (df ^-^ dg)
    (AffineFunc f0 df) * (AffineFunc g0 dg) = AffineFunc (f0*g0) (f0*^dg ^+^ g0*^df)
    negate (AffineFunc f0 df) = AffineFunc (negate f0) (negateV df)
    abs (AffineFunc f0 df) = AffineFunc (abs f0) (signum f0 *^ df) -- TODO: ineffiency: multiplication by 1
    signum (AffineFunc f0 _) = AffineFunc (signum f0) zeroV
    fromInteger x = AffineFunc (fromInteger x) zeroV


--instance () => Num (AffineFunc b (Expr3 p a da v dv)) where
--    (AffineFunc f0 df) + (AffineFunc g0 dg) = AffineFunc (f0+g0) (df ^+^ dg)

