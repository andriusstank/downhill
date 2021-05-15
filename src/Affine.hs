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


-- IDEA: dv is a function of b (AffineFunc' d b = AffineFunc b (d b)) and then we can have VectorSpace instance
-- with Scalar (AffineFunc' b) = AffineFunc' (Scalar b)
data AffineFunc (d :: Type ~> Type) b = AffineFunc b (d@@b)

instance (AdditiveGroup b, AdditiveGroup (d @@ b)) => AdditiveGroup (AffineFunc d b) where
    zeroV = AffineFunc zeroV zeroV
    negateV (AffineFunc y0 dy) = AffineFunc (negateV y0) (negateV dy)
    AffineFunc y0 dy ^-^ AffineFunc z0 dz = AffineFunc (y0 ^-^ z0) (dy ^-^ dz)
    AffineFunc y0 dy ^+^ AffineFunc z0 dz = AffineFunc (y0 ^+^ z0) (dy ^+^ dz)

instance (VectorSpace b, VectorSpace (d @@ b), d @@ Scalar b ~ Scalar b, (d@@b) ~ b) => VectorSpace (AffineFunc d b) where
    type Scalar (AffineFunc d b) = AffineFunc d (Scalar b)
    AffineFunc a da *^ AffineFunc v dv = AffineFunc (a*^v) ( (da*^v) ^+^ (a*^dv))

instance (Num b, VectorSpace (d @@ b), b ~ Scalar (d @@ b)) => Num (AffineFunc d b) where
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

instance (Fractional b, VectorSpace (d @@ b), b ~ Scalar (d @@ b)) => Fractional (AffineFunc d b) where
    fromRational x = AffineFunc (fromRational x) zeroV
    recip (AffineFunc x dx) = AffineFunc (recip x) (df *^ dx)
        where df = negate (recip (sqr x))
    AffineFunc x dx / AffineFunc y dy = AffineFunc (x/y) ((recip y *^ dx) ^-^ ((x/sqr y) *^ dy))

instance (Floating b, VectorSpace (d @@ b), b ~ Scalar (d @@ b)) => Floating (AffineFunc d b) where
    pi = AffineFunc pi zeroV
    exp (AffineFunc x dx) = AffineFunc (exp x) (exp x *^ dx)
    log (AffineFunc x dx) = AffineFunc (log x) (recip x *^ dx)
    sin (AffineFunc x dx) = AffineFunc (sin x) (cos x *^ dx)
    cos (AffineFunc x dx) = AffineFunc (cos x) (negate (sin x) *^ dx)
    asin (AffineFunc x dx) = AffineFunc (asin x) (rsqrt (1 - sqr x) *^ dx)
    acos (AffineFunc x dx) = AffineFunc (acos x) (negate (rsqrt (1 - sqr x)) *^ dx)
    atan (AffineFunc x dx) = AffineFunc (atan x) (recip (1 + sqr x) *^ dx)
    sinh (AffineFunc x dx) = AffineFunc (sinh x) (cosh x *^ dx)
    cosh (AffineFunc x dx) = AffineFunc (cosh x) (sinh x *^ dx)
    asinh (AffineFunc x dx) = AffineFunc (asinh x) (rsqrt (1 + sqr x) *^ dx)
    acosh (AffineFunc x dx) = AffineFunc (acosh x) (rsqrt (sqr x - 1) *^ dx)
    atanh (AffineFunc x dx) = AffineFunc (atanh x) (recip (1 - sqr x) *^ dx)
