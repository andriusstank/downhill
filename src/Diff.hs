{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Diff
(
    -- * ???
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    -- * Lift
    liftFun1, liftFun2, liftFun3,
    -- * Easy lift
    easyLift1, easyLift2, easyLift3,
)
where
import Downhill.Linear.Expr
    ( Expr(ExprSum),
      SparseVector(SparseVector),
      BasicVector(..),
      BackFun(..),
      FullVector(..) )
import Prelude hiding (fst, snd, zip)
import Downhill.DVar (DVar(DVar), dvarValue, constant, var, backprop, BVar)
import Downhill.Linear.BackGrad(BackGrad(..), HasGrad (GradOf), GradBuilder, castNode)
import qualified Downhill.Linear.Lift as Easy
import Downhill.Linear.Lift (LinFun1, LinFun3, lift3)
import qualified Downhill.Linear.Lift as Lift
import Downhill.DVar

type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue = dvarValue

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop x 1

intoFst :: Monoid x => (SparseVector v -> Maybe (VecBuilder v, x))
intoFst (SparseVector dx) = Just (dx, mempty)


liftDenseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, GradOf b -> GradBuilder c)) -> BVar a c -> BVar a b
liftDenseFun1 go = liftSparseFun1 go'
    where go' x = let (y, dy) = go x in (y, dy . sumBuilder)

liftSparseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, VecBuilder (GradOf b) -> VecBuilder (GradOf c))) -> BVar a c -> BVar a b
liftSparseFun1 go (DVar v0 (BackGrad dv)) = DVar y0 (castNode node)
    where f :: SparseVector (GradOf b) -> GradBuilder c
          f = \(SparseVector x) -> goo x
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b))
          node = ExprSum (dv f)
          (y0, goo) = go v0
