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
    castBackGrad,
  )
where

import Data.VectorSpace
  ( AdditiveGroup (..),
    Scalar,
    VectorSpace (..),
  )
import Downhill.Linear.Expr (BasicVector (VecBuilder), Expr (ExprSum), FullVector (identityBuilder, negateBuilder, scaleBuilder), Term (Term))

-- | Linear expression, made for backpropagation.
-- It is similar to @'Expr' 'BackFun'@, but has a more flexible form.
newtype BackGrad a v = BackGrad (forall x. (x -> VecBuilder v) -> [Term a x])

-- | Creates a @BackGrad@ that is backed by a real node. Gradient of type @v@ will be computed for this node.
realNode :: Expr a v -> BackGrad a v
realNode x = BackGrad (\f -> [Term f x])

-- | @inlineNode f x@ will apply function @f@ to variable @x@ without creating a node. All the gradients
-- coming to this expression will be forwarded to the parents of @x@. However, if this expression is used
-- more than once, @f@ will be evaluated multiple times, too. It is intended to be used for @newtype@ wrappers.
-- @inlineNode f x@ also shouldn't prevent
-- compiler to inline and optimize @x@, but I should verify wether this is really the case.
inlineNode :: forall r u v. (VecBuilder v -> VecBuilder u) -> BackGrad r u -> BackGrad r v
inlineNode f (BackGrad g) = BackGrad go
  where
    go :: forall x. (x -> VecBuilder v) -> [Term r x]
    go h = g (f . h)

-- | @BackGrad@ doesn't track the type of the node. Type of @BackGrad@ can be changed freely
-- as long as @VecBuilder@ stays the same.
castBackGrad :: forall r v z. (BasicVector v, VecBuilder z ~ VecBuilder v) => BackGrad r v -> BackGrad r z
castBackGrad (BackGrad g) = BackGrad g


instance (FullVector v) => AdditiveGroup (BackGrad r v) where
  zeroV = BackGrad (const [])
  negateV (BackGrad x) = realNode (ExprSum (x negateBuilder))
  BackGrad x ^+^ BackGrad y = realNode (ExprSum (x identityBuilder <> y identityBuilder))
  BackGrad x ^-^ BackGrad y = realNode (ExprSum (x identityBuilder <> y negateBuilder))

instance FullVector v => VectorSpace (BackGrad r v) where
  type Scalar (BackGrad r v) = Scalar v
  a *^ BackGrad v = realNode (ExprSum (v (scaleBuilder a)))
