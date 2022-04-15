{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Linear.BackGrad
  ( BackGrad (..),
    realNode,
    inlineNode,
    sparseNode,
    castBackGrad,
  )
where

import Data.VectorSpace
  ( AdditiveGroup (..),
    Scalar,
    VectorSpace (..),
  )
import Downhill.Linear.Expr
  ( BasicVector (VecBuilder, identityBuilder),
    Expr (ExprSum),
    Term (Term), SparseVector (unSparseVector),
  )

-- | Linear expression, made for backpropagation.
-- It is similar to @'Expr' 'BackFun'@, but has a more flexible form.
newtype BackGrad a v
  = BackGrad
      ( forall x.
        (x -> VecBuilder v) ->
        Term a x
      )

-- | Creates a @BackGrad@ that is backed by a real node. Gradient of type @v@ will be computed and stored
--   in a graph for this node.
{-# ANN module "HLint: ignore Avoid lambda using `infix`" #-}

realNode :: Expr a v -> BackGrad a v
realNode x = BackGrad (\f -> Term f x)

-- | @inlineNode f x@ will apply function @f@ to variable @x@ without creating a node. All of the gradients
-- coming to this expression will be forwarded to the parents of @x@. However, if this expression is used
-- more than once, @f@ will be evaluated multiple times, too. It is intended to be used for @newtype@ wrappers.
-- @inlineNode f x@ also doesn't prevent
-- compiler to inline and optimize @x@
inlineNode ::
  forall r u v.
  (VecBuilder v -> VecBuilder u) ->
  BackGrad r u ->
  BackGrad r v
inlineNode f (BackGrad g) = BackGrad go
  where
    go :: forall x. (x -> VecBuilder v) -> Term r x
    go h = g (f . h)

sparseNode ::
  forall r a z.
  BasicVector z =>
  (VecBuilder z -> VecBuilder a) ->
  BackGrad r a ->
  BackGrad r z
sparseNode fa (BackGrad x) = castBackGrad (realNode node)
  where
    fa' = fa . unSparseVector
    node :: Expr r (SparseVector z)
    node = ExprSum [x fa']

-- | @BackGrad@ doesn't track the type of the node. Type of @BackGrad@ can be changed freely
-- as long as @VecBuilder@ stays the same.
castBackGrad ::
  forall r v z.
  VecBuilder z ~ VecBuilder v =>
  BackGrad r v ->
  BackGrad r z
castBackGrad (BackGrad g) = BackGrad g

instance (BasicVector v, AdditiveGroup v) => AdditiveGroup (BackGrad r v) where
  zeroV = realNode (ExprSum [])
  negateV (BackGrad x) = realNode (ExprSum [x (identityBuilder . negateV)])
  BackGrad x ^+^ BackGrad y = realNode (ExprSum [x identityBuilder, y identityBuilder])
  BackGrad x ^-^ BackGrad y = realNode (ExprSum [x identityBuilder, y (identityBuilder . negateV)])

instance (BasicVector v, VectorSpace v) => VectorSpace (BackGrad r v) where
  type Scalar (BackGrad r v) = Scalar v
  a *^ BackGrad v = realNode (ExprSum [v (identityBuilder . (a*^))])
