{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Linear.BackGrad
  ( BackGrad (..),
    HasDual (..),
    DualBuilder,
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

-- | Not absolutly required, but it's nice to parameterize expressions based on type
-- of the variable, not on its gradient.
class FullVector (DualOf v) => HasDual v where
  type DualOf v :: Type

  -- @DualOf (Scalar v)@ is normally the same as @Scalar v@. Unless we
  -- attempt to backpropagate on something else than the scalar.
  evalGrad :: DualOf v -> v -> DualOf (Scalar v)

type DualBuilder v = VecBuilder (DualOf v)

type SparseGrad v = SparseVector (DualOf v)

instance HasDual Float where
  type DualOf Float = Float
  evalGrad = (*)

instance HasDual Double where
  type DualOf Double = Double
  evalGrad = (*)

instance
  ( HasDual u,
    HasDual v,
    da ~ Scalar (DualOf u),
    da ~ Scalar (DualOf v),
    a ~ Scalar u,
    a ~ Scalar v,
    AdditiveGroup (DualOf a)
  ) =>
  HasDual (u, v)
  where
  type DualOf (u, v) = (DualOf u, DualOf v)
  evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

instance
  ( HasDual u,
    HasDual v,
    HasDual w,
    a ~ Scalar (DualOf u),
    a ~ Scalar (DualOf v),
    a ~ Scalar (DualOf w),
    a ~ Scalar u,
    a ~ Scalar v,
    a ~ Scalar w,
    AdditiveGroup (DualOf a)
  ) =>
  HasDual (u, v, w)
  where
  type DualOf (u, v, w) = (DualOf u, DualOf v, DualOf w)
  evalGrad (a, b, c) (x, y, z) = evalGrad a x ^+^ evalGrad b y ^+^ evalGrad c z

-- | @BackGrad@ is a basic block for building computational graph of linear functions.
-- @BackGrad a v@ is similar to @'Expr' 'BackFun' ('DualOf' a) ('DualOf' v)@, but it has a more
-- flexible form. It encapsulates the type of the gradient of @v@, which can be different from @DualOf v@
-- and can be chosen independently for each use.
newtype BackGrad a v = BackGrad (forall x. (x -> DualBuilder v) -> [Term BackFun (DualOf a) x])

-- | Creates a @BackGrad@ that is backed by a real node. Gradient of type '@DualOf@ v' will be computed for this node.
realNode :: Expr BackFun (DualOf a) (DualOf v) -> BackGrad a v
realNode x = BackGrad (\f -> [Term (BackFun f) x])

-- | Type of a node can be changed freely, as long as its @VecBuilder@ stays the same.
castNode :: forall r dv z. (BasicVector dv, DualBuilder z ~ VecBuilder dv) => Expr BackFun (DualOf r) dv -> BackGrad r z
castNode node = BackGrad go
  where
    go :: forall x. (x -> DualBuilder z) -> [Term BackFun (DualOf r) x]
    go g = [Term (BackFun g) node]

-- | @inlineNode f x@ will apply function @f@ to variable @x@ without creating a node. All the gradients
-- coming to this expression will be forwarded to the parents of @x@. However, if this expression is used
-- more than once, @f@ will be evaluated multiple times, too. It is intended to be used for @newtype@ wrappers.
-- @inlineNode f x@ also shouldn't prevent
-- compiler to inline and optimize @x@, but I should verify wether this is really the case.
inlineNode :: forall a u v. (DualBuilder v -> DualBuilder u) -> BackGrad a u -> BackGrad a v
inlineNode f (BackGrad g) = BackGrad go
  where
    go :: forall x. (x -> DualBuilder v) -> [Term BackFun (DualOf a) x]
    go h = g (f . h)

instance (HasDual v) => AdditiveGroup (BackGrad a v) where
  zeroV = BackGrad (const [])
  negateV (BackGrad x) = realNode (ExprSum (x negateBuilder))
  BackGrad x ^+^ BackGrad y = realNode (ExprSum (x identityBuilder <> y identityBuilder))
  BackGrad x ^-^ BackGrad y = realNode (ExprSum (x identityBuilder <> y negateBuilder))

instance HasDual v => VectorSpace (BackGrad a v) where
  type Scalar (BackGrad a v) = Scalar (DualOf v)
  a *^ BackGrad v = realNode (ExprSum (v (scaleBuilder a)))
