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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Downhill.DVar (
    DVar(..),
    dvarValue,
    -- * BVar
    BVar,
    var, constant, backprop
)
where

import Data.AdditiveGroup (AdditiveGroup)
import Data.VectorSpace
    ( VectorSpace((*^)), Scalar, VectorSpace(..), AdditiveGroup(..) )
import Data.Kind ( Type )
import Prelude hiding (id, (.))
import Downhill.Linear.Expr (FullVector (identityBuilder), Expr (ExprSum, ExprVar), BackFun, Term, BasicVector)
import Downhill.Linear.BackGrad
    ( castNode, BackGrad(..), HasGrad(..), realNode )
import qualified Downhill.Linear.Graph as Graph

data DVar (d :: Type -> Type) a = DVar a (d a)

dvarValue :: DVar d a -> a
dvarValue (DVar x _) = x


instance (AdditiveGroup b, AdditiveGroup (d b)) => AdditiveGroup (DVar d b) where
    zeroV = DVar zeroV zeroV
    negateV (DVar y0 dy) = DVar (negateV y0) (negateV dy)
    DVar y0 dy ^-^ DVar z0 dz = DVar (y0 ^-^ z0) (dy ^-^ dz)
    DVar y0 dy ^+^ DVar z0 dz = DVar (y0 ^+^ z0) (dy ^+^ dz)

instance (Num b, VectorSpace (d b), b ~ Scalar (d b)) => Num (DVar d b) where
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

instance (Fractional b, VectorSpace (d b), b ~ Scalar (d b)) => Fractional (DVar d b) where
    fromRational x = DVar (fromRational x) zeroV
    recip (DVar x dx) = DVar (recip x) (df *^ dx)
        where df = negate (recip (sqr x))
    DVar x dx / DVar y dy = DVar (x/y) ((recip y *^ dx) ^-^ ((x/sqr y) *^ dy))

instance (Floating b, VectorSpace (d b), b ~ Scalar (d b)) => Floating (DVar d b) where
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
  ( VectorSpace v
  , VectorSpace (GradOf v)
  , FullVector (GradOf (Scalar v))
  , Scalar (GradOf v) ~ Scalar v
  , FullVector (GradOf v)
  , HasGrad v
  ) => VectorSpace (DVar (BackGrad a) v) where
    type Scalar (DVar (BackGrad a) v) = DVar (BackGrad a) (Scalar v)
    DVar a (BackGrad da) *^ DVar v (BackGrad dv) = DVar (a *^ v) (castNode node)
        where node :: Expr BackFun (GradOf a) (GradOf v)
              node = ExprSum (term1 ++ term2)
                where term1 :: [Term BackFun (GradOf a) (GradOf v)]
                      term1  = da (\v' -> identityBuilder (evalGrad v' v))
                      term2 :: [Term BackFun (GradOf a) (GradOf v)]
                      term2 = dv (\v' -> identityBuilder (a *^ v'))

type BVar a v = DVar (BackGrad a) v

constant :: a -> BVar a a
constant x = DVar x (BackGrad (const []))

var :: a -> BVar a a
var x = DVar x (realNode ExprVar)

backprop :: forall a v. (FullVector (GradOf v), BasicVector (GradOf a)) => BVar a v -> GradOf v -> GradOf a
backprop (DVar _y0 x) = Graph.backprop x
