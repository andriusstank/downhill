{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language ScopedTypeVariables #-}

module Downhill.Linear.BackGrad
(
    HasGrad(..),
    GradBuilder, SparseGrad,
    BackGrad(..),
    realNode, castNode, inlineNode
)
where

import Downhill.Linear.Expr (Term(Term), FullVector (negateBuilder, identityBuilder, scaleBuilder), BasicVector (VecBuilder), SparseVector, BackFun(BackFun), Expr (ExprSum))
import Data.VectorSpace
    ( Scalar, VectorSpace(..), AdditiveGroup(..) )
import Data.Kind (Type)
import Affine (DVar(DVar))

class FullVector (GradOf v) => HasGrad v where
    type GradOf v :: Type
    evalGrad :: GradOf v -> v -> GradOf (Scalar v)

type GradBuilder v = VecBuilder (GradOf v)

type SparseGrad v = SparseVector (GradOf v)

instance HasGrad Float where
    type GradOf Float = Float
    evalGrad = (*)

instance HasGrad Double where
    type GradOf Double = Double
    evalGrad = (*)

instance (HasGrad u, HasGrad v, Scalar (GradOf u) ~ Scalar (GradOf v), a ~ Scalar u, a ~ Scalar v, AdditiveGroup (GradOf a)) => HasGrad (u, v) where
    type GradOf (u, v) = (GradOf u, GradOf v)
    evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y


newtype BackGrad a v = BackGrad (forall x. (x -> GradBuilder v) -> [Term BackFun (GradOf a) x])

realNode :: Expr BackFun (GradOf a) (GradOf v) -> BackGrad a v
realNode x = BackGrad (\f -> [Term (BackFun f) x])

castNode :: forall r dv z. (BasicVector dv, GradBuilder z ~ VecBuilder dv) => Expr BackFun (GradOf r) dv -> BackGrad r z
castNode node = BackGrad go
    where go :: forall x. (x -> GradBuilder z) -> [Term BackFun (GradOf r) x]
          go g = [Term (BackFun g) node]

inlineNode :: forall a u v. (GradBuilder v -> GradBuilder u) -> BackGrad a u -> BackGrad a v
inlineNode f (BackGrad g) = BackGrad go
    where go :: forall x. (x -> GradBuilder v) -> [Term BackFun (GradOf a) x]
          go h = g (f . h)

instance (HasGrad v) => AdditiveGroup (BackGrad a v) where
    zeroV = BackGrad (const [])
    negateV (BackGrad x) = realNode (ExprSum (x negateBuilder))
    BackGrad x ^+^ BackGrad y = realNode (ExprSum (x identityBuilder <> y identityBuilder))
    BackGrad x ^-^ BackGrad y = realNode (ExprSum (x identityBuilder <> y negateBuilder))

instance HasGrad v => VectorSpace (BackGrad a v) where
    type Scalar (BackGrad a v) = Scalar (GradOf v)
    a *^ BackGrad v = realNode (ExprSum (v (scaleBuilder a)))

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
