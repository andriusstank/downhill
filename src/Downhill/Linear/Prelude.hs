{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Downhill.Linear.Prelude
( fst, snd, zip
)
where
import Prelude ((++), (.), Monoid (mempty), Maybe (Just))
import qualified Prelude
import Downhill.Linear.BackGrad (BackGrad(BackGrad), SparseGrad, HasGrad (GradOf), castNode, GradBuilder)
import Downhill.Linear.Expr (Expr (ExprSum), BackFun, SparseVector (SparseVector, unSparseVector), BasicVector (VecBuilder), maybeToMonoid)

fst :: forall r b1 b2. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BackGrad r (b1, b2) -> BackGrad r b1
fst (BackGrad dv) = castNode node
    where f :: SparseVector (GradOf b1) -> GradBuilder (b1, b2)
          f (SparseVector x) = Just (x, mempty)
          node :: Expr BackFun (GradOf r) (SparseVector (GradOf b1))
          node = ExprSum (dv f)

snd :: forall r b1 b2. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BackGrad r (b1, b2) -> BackGrad r b2
snd (BackGrad dv) = castNode node
    where f :: SparseVector (GradOf b2) -> GradBuilder (b1, b2)
          f (SparseVector x) = Just (mempty, x)
          node :: Expr BackFun (GradOf r) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

zip :: forall r b1 b2. (HasGrad b1, HasGrad b2) => BackGrad r b1 -> BackGrad r b2 -> BackGrad r (b1, b2)
zip (BackGrad da) (BackGrad db) = castNode node
    where node :: Expr BackFun (GradOf r) (SparseGrad (b1, b2))
          node = ExprSum (da fa ++ db fb)
          fa :: SparseVector (GradOf b1, GradOf b2) -> VecBuilder (GradOf b1)
          fa = Prelude.fst . maybeToMonoid . unSparseVector
          fb :: SparseVector (GradOf b1, GradOf b2) -> VecBuilder (GradOf b2)
          fb = Prelude.snd . maybeToMonoid . unSparseVector
