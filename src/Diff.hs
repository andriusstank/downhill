{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Diff
  ( -- * ???
    BVar,
    BVarS,
    bvarValue,
    constant,
    var,
    backprop,
    backpropS,

    -- * Lift
    liftFun1,
    liftFun2,
    liftFun3,

    -- * Easy lift
    easyLift1,
    easyLift2,
    easyLift3,
  )
where

import Downhill.DVar
import Downhill.Linear.BackGrad (BackGrad (..), GradBuilder, HasGrad (GradOf), castNode)
import Downhill.Linear.Expr
  ( BackFun,
    BasicVector (VecBuilder, sumBuilder),
    Expr (ExprSum),
    FullVector,
    SparseVector (SparseVector),
  )
import Prelude hiding (fst, snd, zip)

type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue = dvarValue

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop x 1

intoFst :: Monoid x => (SparseVector v -> Maybe (VecBuilder v, x))
intoFst (SparseVector dx) = Just (dx, mempty)

liftDenseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, GradOf b -> GradBuilder c)) -> BVar a c -> BVar a b
liftDenseFun1 go = liftSparseFun1 go'
  where
    go' x = let (y, dy) = go x in (y, dy . sumBuilder)

liftSparseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, VecBuilder (GradOf b) -> VecBuilder (GradOf c))) -> BVar a c -> BVar a b
liftSparseFun1 go (DVar v0 (BackGrad dv)) = DVar y0 (castNode node)
  where
    f :: SparseVector (GradOf b) -> GradBuilder c
    f = \(SparseVector x) -> goo x
    node :: Expr BackFun (GradOf a) (SparseVector (GradOf b))
    node = ExprSum (dv f)
    (y0, goo) = go v0
