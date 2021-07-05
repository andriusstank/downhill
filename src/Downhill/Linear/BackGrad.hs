{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Linear.BackGrad
  ( BackGrad (..),
    HasGrad (..),
    GradBuilder,
    SparseGrad,
    realNode,
    castNode,
    inlineNode,
  )
where

import Data.Kind (Type)
import Data.VectorSpace
  ( AdditiveGroup (..),
    Scalar,
    VectorSpace (..),
  )
import Downhill.Linear.Expr (BackFun (BackFun), BasicVector (VecBuilder), Expr (ExprSum), FullVector (identityBuilder, negateBuilder, scaleBuilder), SparseVector, Term (Term))

-- TODO: HasGrad v -> Dual v, HasGrad (Needle p) -> HasGrad p

-- | Not absolutly required, but it's nice to parameterize expressions based on type
-- of the variable, not on its gradient.
class FullVector (GradOf v) => HasGrad v where
  type GradOf v :: Type

  -- @GradOf (Scalar v)@ is normally the same as @Scalar v@. Unless we
  -- attempt to backpropagate on something else than the scalar.
  evalGrad :: GradOf v -> v -> GradOf (Scalar v)

type GradBuilder v = VecBuilder (GradOf v)

type SparseGrad v = SparseVector (GradOf v)

instance HasGrad Float where
  type GradOf Float = Float
  evalGrad = (*)

instance HasGrad Double where
  type GradOf Double = Double
  evalGrad = (*)

instance
  ( HasGrad u,
    HasGrad v,
    da ~ Scalar (GradOf u),
    da ~ Scalar (GradOf v),
    a ~ Scalar u,
    a ~ Scalar v,
    AdditiveGroup (GradOf a)
  ) =>
  HasGrad (u, v)
  where
  type GradOf (u, v) = (GradOf u, GradOf v)
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance
  ( HasGrad u,
    HasGrad v,
    HasGrad w,
    a ~ Scalar (GradOf u),
    a ~ Scalar (GradOf v),
    a ~ Scalar (GradOf w),
    a ~ Scalar u,
    a ~ Scalar v,
    a ~ Scalar w,
    AdditiveGroup (GradOf a)
  ) =>
  HasGrad (u, v, w)
  where
  type GradOf (u, v, w) = (GradOf u, GradOf v, GradOf w)
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

-- | @BackGrad@ is a basic block for building computational graph of linear functions.
-- @BackGrad a v@ is similar to @'Expr' 'BackFun' ('GradOf' a) ('GradOf' v)@, but it has a more
-- flexible form. It encapsulates the type of the gradient of @v@, which can be different from @GradOf v@
-- and can be chosen independently for each use.
newtype BackGrad a v = BackGrad (forall x. (x -> GradBuilder v) -> [Term BackFun (GradOf a) x])

-- | Creates a @BackGrad@ that is backed by a real node. Gradient of type '@GradOf@ v' will be computed for this node.
realNode :: Expr BackFun (GradOf a) (GradOf v) -> BackGrad a v
realNode x = BackGrad (\f -> [Term (BackFun f) x])

-- | Type of a node can be changed freely, as long as its @VecBuilder@ stays the same.
castNode :: forall r dv z. (BasicVector dv, GradBuilder z ~ VecBuilder dv) => Expr BackFun (GradOf r) dv -> BackGrad r z
castNode node = BackGrad go
  where
    go :: forall x. (x -> GradBuilder z) -> [Term BackFun (GradOf r) x]
    go g = [Term (BackFun g) node]

-- | @inlineNode f x@ will apply function @f@ to variable @x@ without creating a node. All the gradients
-- coming to this expression will be forwarded to the parents of @x@. However, if this expression is used
-- more than once, @f@ will be evaluated multiple times, too. It is intended to be used for @newtype@ wrappers.
-- @inlineNode f x@ also shouldn't prevent
-- compiler to inline and optimize @x@, but I should verify wether this is really the case.
inlineNode :: forall a u v. (GradBuilder v -> GradBuilder u) -> BackGrad a u -> BackGrad a v
inlineNode f (BackGrad g) = BackGrad go
  where
    go :: forall x. (x -> GradBuilder v) -> [Term BackFun (GradOf a) x]
    go h = g (f . h)

instance (HasGrad v) => AdditiveGroup (BackGrad a v) where
  zeroV = BackGrad (const [])
  negateV (BackGrad x) = realNode (ExprSum (x negateBuilder))
  BackGrad x ^+^ BackGrad y = realNode (ExprSum (x identityBuilder <> y identityBuilder))
  BackGrad x ^-^ BackGrad y = realNode (ExprSum (x identityBuilder <> y negateBuilder))

instance HasGrad v => VectorSpace (BackGrad a v) where
  type Scalar (BackGrad a v) = Scalar (GradOf v)
  a *^ BackGrad v = realNode (ExprSum (v (scaleBuilder a)))
