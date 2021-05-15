{-# LANGUAGE DerivingVia #-}
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
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Affine where

import Data.AffineSpace (AffineSpace((.-.), Diff, (.+^)))
import Data.AdditiveGroup (AdditiveGroup((^+^)))
import Data.VectorSpace
    ( AdditiveGroup((^-^), negateV, zeroV),
      VectorSpace(Scalar, (*^)),
      sumV )
import Notensor (BasicVector (VecBuilder, sumBuilder'), FullVector(..), Dense(..), BackFun(..), NumBuilder (NumBuilder, unNumBuilder))
import EType (Endpoint (InnerNode, SourceNode), Node(..),Edge (Edge))
import Data.Kind
import Data.Constraint (Dict(Dict))
import Expr (Expr(..), Term(..))
import Control.Category (Category(..))
import Prelude hiding (id, (.))
import Data.Singletons(type (~>), type (@@), TyCon1, Apply)
import Data.Proxy (Proxy(Proxy))


data DVar (d :: Type ~> Type) b = DVar b (d@@b)

instance (AdditiveGroup b, AdditiveGroup (d @@ b)) => AdditiveGroup (DVar d b) where
    zeroV = DVar zeroV zeroV
    negateV (DVar y0 dy) = DVar (negateV y0) (negateV dy)
    DVar y0 dy ^-^ DVar z0 dz = DVar (y0 ^-^ z0) (dy ^-^ dz)
    DVar y0 dy ^+^ DVar z0 dz = DVar (y0 ^+^ z0) (dy ^+^ dz)

{-
instance (VectorSpace v, VectorSpace (d @@ v)) => VectorSpace (DVar d v) where
    type Scalar (DVar d v) = DVar d (Scalar v)
    DVar a da *^ DVar v dv = DVar (a*^v) ( (_ da v) ^+^ (a*^dv))
-}

instance (Num b, VectorSpace (d @@ b), b ~ Scalar (d @@ b)) => Num (DVar d b) where
    (DVar f0 df) + (DVar g0 dg) = DVar (f0+g0) (df ^+^ dg)
    (DVar f0 df) - (DVar g0 dg) = DVar (f0-g0) (df ^-^ dg)
    (DVar f0 df) * (DVar g0 dg) = DVar (f0*g0) (f0*^dg ^+^ g0*^df)
    negate (DVar f0 df) = DVar (negate f0) (negateV df)
    abs (DVar f0 df) = DVar (abs f0) (signum f0 *^ df) -- TODO: ineffiency: multiplication by 1
    signum (DVar f0 _) = DVar (signum f0) zeroV
    fromInteger x = DVar (fromInteger x) zeroV


sqr :: Num a => a -> a
sqr x = x*x

rsqrt :: Floating a => a -> a
rsqrt x = recip (sqrt x)

instance (Fractional b, VectorSpace (d @@ b), b ~ Scalar (d @@ b)) => Fractional (DVar d b) where
    fromRational x = DVar (fromRational x) zeroV
    recip (DVar x dx) = DVar (recip x) (df *^ dx)
        where df = negate (recip (sqr x))
    DVar x dx / DVar y dy = DVar (x/y) ((recip y *^ dx) ^-^ ((x/sqr y) *^ dy))

instance (Floating b, VectorSpace (d @@ b), b ~ Scalar (d @@ b)) => Floating (DVar d b) where
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
