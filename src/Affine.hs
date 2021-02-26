{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Affine where

import Data.AffineSpace (AffineSpace((.-.), Diff))
import Tensor (TensorProduct(..))
import Data.AdditiveGroup (AdditiveGroup((^+^)))
import Data.VectorSpace (AdditiveGroup((^-^), negateV, zeroV), VectorSpace(Scalar, (*^)))
import Notensor (BasicVector (VecBuilder, sumBuilder), FullVector(..), ProdVector(..), Dense(..))
import EType (Endpoint)
import Data.VectorSpace (sumV)

newtype AsNum a = AsNum { unAsNum :: a }
    deriving Show
    deriving Num via a
    deriving Fractional via a
    deriving Floating via a

instance Num a => AdditiveGroup (AsNum a) where
    zeroV = 0
    (^+^) = (+)
    (^-^) = (-)
    negateV = negate

instance Num a => VectorSpace (AsNum a) where
    type Scalar (AsNum a) = AsNum a
    (*^) = (*)

   
data AffineFunc b dv = AffineFunc b dv

instance (AdditiveGroup b, AdditiveGroup f) => BasicVector (AffineFunc b f) where
    type VecBuilder (AffineFunc b f) = AffineFunc b f
    sumBuilder = sumV

instance (AdditiveGroup b, AdditiveGroup f) => ProdVector (AffineFunc b f) where
  zeroBuilder = zeroV
  identityBuilder = id

class VectorSpace dv => LinearFunc dv where
    identityFunc :: dv
    scaleFunc :: Scalar dv -> dv -- scaleFunc x == x *^ identityFunc
    sumF :: [dv] -> dv

   
evalAffineFunc :: (AdditiveGroup (dv ⊗ v), TensorProduct dv v) => AffineFunc (dv ⊗ v) dv -> v -> dv ⊗ v
evalAffineFunc (AffineFunc y0 dydx) x = y0 ^+^ (dydx ⊗ x)

instance (AdditiveGroup b, AdditiveGroup dv) => AdditiveGroup (AffineFunc b dv) where
    zeroV = AffineFunc zeroV zeroV
    negateV (AffineFunc y0 dy) = AffineFunc (negateV y0) (negateV dy)
    AffineFunc y0 dy ^-^ AffineFunc z0 dz = AffineFunc (y0 ^-^ z0) (dy ^-^ dz)
    AffineFunc y0 dy ^+^ AffineFunc z0 dz = AffineFunc (y0 ^+^ z0) (dy ^+^ dz)

instance (Num b, VectorSpace dv, b ~ Scalar dv) => Num (AffineFunc b dv) where
    (AffineFunc f0 df) + (AffineFunc g0 dg) = AffineFunc (f0+g0) (df ^+^ dg)
    (AffineFunc f0 df) - (AffineFunc g0 dg) = AffineFunc (f0-g0) (df ^-^ dg)
    (AffineFunc f0 df) * (AffineFunc g0 dg) = AffineFunc (f0*g0) (f0*^dg ^+^ g0*^df)
    negate (AffineFunc f0 df) = AffineFunc (negate f0) (negateV df)
    abs (AffineFunc f0 df) = AffineFunc (abs f0) (signum f0 *^ df) -- TODO: ineffiency: multiplication by 1
    signum (AffineFunc f0 _) = AffineFunc (signum f0) zeroV
    fromInteger x = AffineFunc (fromInteger x) zeroV

sqr :: Num a => a -> a
sqr x = x*x

rsqrt :: Floating a => a -> a
rsqrt x = recip (sqrt x)

instance (Fractional b, VectorSpace dv, b ~ Scalar dv) => Fractional (AffineFunc b dv) where
    fromRational x = AffineFunc (fromRational x) zeroV
    recip (AffineFunc x dx) = AffineFunc (recip x) (df *^ dx)
        where df = negate (recip (sqr x))
    AffineFunc x dx / AffineFunc y dy = AffineFunc (x/y) ((recip y *^ dx) ^-^ ((x/sqr y) *^ dy))

instance (Floating b, VectorSpace dv, b ~ Scalar dv) => Floating (AffineFunc b dv) where
    pi = AffineFunc pi zeroV
    exp (AffineFunc x dx) = AffineFunc (exp x) (exp x *^ dx)
    log (AffineFunc x dx) = AffineFunc (log x) (recip x *^ dx)
    sin (AffineFunc x dx) = AffineFunc (sin x) (cos x *^ dx)
    cos (AffineFunc x dx) = AffineFunc (cos x) (negate (sin x) *^ dx)
    asin (AffineFunc x dx) = AffineFunc (asin x) ((rsqrt (1 - sqr x)) *^ dx)
    acos (AffineFunc x dx) = AffineFunc (acos x) (negate (rsqrt (1 - sqr x)) *^ dx)
    atan (AffineFunc x dx) = AffineFunc (atan x) (recip (1 + sqr x) *^ dx)
    sinh (AffineFunc x dx) = AffineFunc (sinh x) (cosh x *^ dx)
    cosh (AffineFunc x dx) = AffineFunc (cosh x) (sinh x *^ dx)
    asinh (AffineFunc x dx) = AffineFunc (asinh x) (rsqrt (1 + sqr x) *^ dx)
    acosh (AffineFunc x dx) = AffineFunc (acosh x) (rsqrt (sqr x - 1) *^ dx)
    atanh (AffineFunc x dx) = AffineFunc (atanh x) (recip (1 - sqr x) *^ dx)

--instance () => Num (AffineFunc b (Expr3 p a da v dv)) where
--    (AffineFunc f0 df) + (AffineFunc g0 dg) = AffineFunc (f0+g0) (df ^+^ dg)

