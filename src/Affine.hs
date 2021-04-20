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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Affine where

import Data.AffineSpace (AffineSpace((.-.), Diff, (.+^)))
import Tensor (Bilinear(..))
import Data.AdditiveGroup (AdditiveGroup((^+^)))
import Data.VectorSpace (AdditiveGroup((^-^), negateV, zeroV), VectorSpace(Scalar, (*^)))
import Notensor (BasicVector (VecBuilder, sumBuilder), FullVector(..), ProdVector(..), Dense(..), BackFunc, scaleFunc, LinearEdge(..), NumBuilder (NumBuilder, unNumBuilder))
import EType (Endpoint (InnerNode, SourceNode), Node(..),Edge (Edge))
import Data.VectorSpace (sumV)
import Data.Kind
import Data.Constraint (Dict(Dict))
import Expr (Expr(..), Term(..))
import Control.Category (Category(..))
import Prelude hiding (id, (.))


   
-- IDEA: dv is a function of b (AffineFunc' d b = AffineFunc b (d b)) and then we can have VectorSpace instance
-- with Scalar (AffineFunc' b) = AffineFunc' (Scalar b)
data AffineFunc b dv = AffineFunc b dv

class VectorSpace dv => LinearFunc dv where
    identityFunc :: dv
    scaleFunc' :: Scalar dv -> dv -- scaleFunc x == x *^ identityFunc
    sumF :: [dv] -> dv
   
evalAffineFunc :: (AdditiveGroup (dv ✕ v), Bilinear dv v) => AffineFunc (dv ✕ v) dv -> v -> dv ✕ v
evalAffineFunc (AffineFunc y0 dydx) x = y0 ^+^ (dydx ✕ x)

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

-- instance Num b =BasicVector (AffineFunc (AsNum b) (AsNum dv)) where
--     type VecBuilder (AffineFunc (AsNum b) (AsNum dv)) = AffineFunc (AsNum b) (AsNum dv)
--     sumBuilder = sumV

-- instance ProdVector (AffineFunc (AsNum b) (AsNum dv)) where

-- instance FullVector (AffineFunc (AsNum b) (AsNum dv)) where


--instance () => Num (AffineFunc b (Expr3 p a da v dv)) where
--    (AffineFunc f0 df) + (AffineFunc g0 dg) = AffineFunc (f0+g0) (df ^+^ dg)

-- a -> b
data AffineFunc2 a dv = AffineFunc2 (dv ✕ a) dv

evalAffineFunc2 :: forall a dv. (AdditiveGroup (dv ✕ a), Bilinear dv a) => AffineFunc2 a dv -> a -> (dv ✕ a)
evalAffineFunc2 (AffineFunc2 y0 dydx) x = y0 ^+^ (dydx ✕ x)

-- u -> du ✕ u . v -> dv ✕ v
-- z0 + dz*y0 + dz*dy*x
composeAffineFunc
  :: forall u v w dv dw. (v ~ (dv ✕ u), w ~ (dw ✕ v), w ~ (dw ✕ dv ✕ u), AdditiveGroup w, Bilinear dw v, Bilinear dw dv)
  => AffineFunc2 v dw -> AffineFunc2 u dv -> AffineFunc2 u (dw ✕ dv)
composeAffineFunc fz@(AffineFunc2 _z0 dz) (AffineFunc2 y0 dy) = AffineFunc2 z0' (dz ✕ dy)
    where y0' :: v
          y0' = y0
          z0' = evalAffineFunc2 fz y0'
--composeAffineFunc (AffineFunc2 z0 dz) (AffineFunc2 y0 dy) = AffineFunc2 (z0 ^+^ (dz ✕ y0)) (dz ✕ dy)

data AffineFunc3 f a b = AffineFunc3 b (f a (Diff b))

class LinearFunc3 (f :: Type -> Type -> Type) where
    type LinearCtx f :: Type -> Constraint 
    bilinearDict :: forall a b. LinearCtx f b => Dict (Bilinear (f a b) a, b ~ (f a b ✕ a))

data FwdGrad a v where
    FwdGrad :: v -> FwdGrad (Scalar v) v

instance VectorSpace v => Bilinear (FwdGrad a v) a where
    type FwdGrad a v ✕ a = v
    FwdGrad v ✕ a = a *^ v

instance LinearFunc3 FwdGrad where
    type LinearCtx FwdGrad = VectorSpace
    bilinearDict = Dict

evalAffineFunc3 :: forall a b f. (AffineSpace b, LinearFunc3 f, LinearCtx f (Diff b)) => AffineFunc3 f a b -> a -> b
evalAffineFunc3 (AffineFunc3 b f) a = case bilinearDict @f @a @(Diff b) of
    Dict -> b .+^ (f ✕ a)

unaryAfFunc :: BasicVector (Diff b) => b -> e a (Diff b) -> AffineFunc3 (Expr e) a b
unaryAfFunc x dx = AffineFunc3 x (ExprSum [ Term dx ExprVar ])

scalarFunc :: (v ~ Diff p, a ~ Scalar v, FullVector v, LinearEdge e) => p -> a -> AffineFunc3 (Expr e) v p
scalarFunc fx dfx = unaryAfFunc fx (scaleFunc dfx)

data ScalarEdge u v where
    IdentityScalarEdge :: ScalarEdge v v
    NegateScalarEdge :: ScalarEdge v v
    ScaleScalarEdge :: Scalar v -> ScalarEdge v v

sinAff :: forall a e. (a ~ Scalar a, Diff a ~ a, FullVector a, Floating a, LinearEdge e) => a -> AffineFunc3 (Expr e) a a
sinAff x = scalarFunc (sin x) (cos x)

sinScalar :: (a ~ Scalar a, Diff a ~ a, FullVector a, Floating a) => a -> AffineFunc3 ScalarEdge a a
sinScalar x = AffineFunc3 (sin x) (ScaleScalarEdge $ cos x)

instance (LinearEdge e, AdditiveGroup v, FullVector (Diff v)) => AdditiveGroup (AffineFunc3 (Expr e) a v) where
    zeroV = AffineFunc3 zeroV zeroV
    negateV (AffineFunc3 x0 dx) = AffineFunc3 (negateV x0) (negateV dx)
    AffineFunc3 x0 dx ^+^ AffineFunc3 y0 dy = AffineFunc3 (x0 ^+^ y0) (dx ^+^ dy)
    AffineFunc3 x0 dx ^-^ AffineFunc3 y0 dy = AffineFunc3 (x0 ^+^ y0) (dx ^-^ dy)

