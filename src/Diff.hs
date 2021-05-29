{-# LANGUAGE LambdaCase #-}
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

{-# LANGUAGE TypeApplications #-}
module Diff
(
    -- * ???
    HasGrad(..),
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    -- * Lift
    liftFun1, liftFun2, liftFun3,
    -- * Easy lift
    easyLift1, easyLift2, easyLift3,
    -- * Prelude
    fst, snd, zip,
)
where
import Downhill.Linear.Expr
    ( Expr(ExprSum),
      SparseVector(SparseVector),
      BasicVector(..),
      BackFun(..),
      FullVector(..) )
import Prelude hiding (fst, snd, zip)
import qualified Prelude
import Downhill.DVar (DVar(DVar), dvarValue, constant, var, backprop, BVar)
import Downhill.Linear.BackGrad(BackGrad(..), HasGrad (GradOf, evalGrad), GradBuilder, castNode)
import qualified Downhill.Linear.EasyLift as Easy
import qualified Downhill.Linear.Prelude as Linear

type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue = dvarValue

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop x 1

intoFst :: Monoid x => (SparseVector v -> Maybe (VecBuilder v, x))
intoFst (SparseVector dx) = Just (dx, mempty)

fst :: forall r b1 b2. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar r (b1, b2) -> BVar r b1
fst (DVar x dx) = DVar (Prelude.fst x) (Linear.fst dx)

snd :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b2
snd (DVar x dx) = DVar (Prelude.snd x) (Linear.snd dx)

data DFunc1 a b = forall x. DFunc1 (a -> (b, VecBuilder b -> x, x -> VecBuilder a))
data DFunc2 a b c = forall x. (BasicVector x, VecBuilder x ~ VecBuilder (GradOf c)) => DFunc2 (a -> b -> (c, x -> VecBuilder (GradOf a), x -> VecBuilder (GradOf b)))

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

liftFun1
    :: forall x r a z. (BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> (z, x -> GradBuilder a))
    -> BVar r a -> BVar r z
liftFun1 dfun (DVar a0 (BackGrad da)) = DVar z0 (castNode node)
    where (z0, fa) = dfun a0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa)


liftFun2
    :: forall x r a b z. (BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> b -> (z, x -> GradBuilder a, x -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r z
liftFun2 dfun (DVar a0 (BackGrad da)) (DVar b0 (BackGrad db)) = DVar z0 (castNode node)
    where (z0, fa, fb) = dfun a0 b0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa ++ db fb)

data EasyLin3 a b c where
    EasyLin3 :: (GradOf z -> (GradBuilder a, GradBuilder b, GradBuilder c)) -> EasyLin3 a b c

data LinFun3 a b c z where
    LinFun3 :: (BasicVector x, VecBuilder x ~ GradBuilder z) => (x -> GradBuilder a) -> (x -> GradBuilder b) -> (x -> GradBuilder c) -> LinFun3 a b c z

linLift3 :: forall r a b c z. ()
    => LinFun3 a b c z
    -> BackGrad r a -> BackGrad r b -> BackGrad r c -> BackGrad r z
linLift3 (LinFun3 fa fb fc) (BackGrad da) (BackGrad db) (BackGrad dc) = castNode node
    where node = ExprSum (da fa ++ db fb ++ dc fc)

liftFun3
    :: forall x r a b c z. (BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> b -> c -> (z, LinFun3 a b c z))
    -> BVar r a -> BVar r b -> BVar r c -> BVar r z
liftFun3 dfun (DVar a0 da) (DVar b0 db) (DVar c0 dc) = DVar z0 (linLift3 f3 da db dc)
    where (z0, f3) = dfun a0 b0 c0

easyLift1
    :: BasicVector (GradOf z)
    => (a -> (z, GradOf z -> GradBuilder a))
    -> BVar r a -> BVar r z
easyLift1 f (DVar a da) = DVar z (Easy.lift1 df da)
    where (z, df) = f a

easyLift2
    :: BasicVector (GradOf z)
    => (a -> b -> (z, GradOf z -> (GradBuilder a, GradBuilder b)))
    -> BVar r a -> BVar r b -> BVar r z
easyLift2 f (DVar a da) (DVar b db) = DVar z (Easy.lift2 df da db)
    where (z, df) = f a b

easyLift3
    :: BasicVector (GradOf z)
    => (a -> b -> c -> (z, GradOf z -> (GradBuilder a, GradBuilder b, GradBuilder c)))
    -> BVar r a -> BVar r b -> BVar r c -> BVar r z
easyLift3 f (DVar a da) (DVar b db) (DVar c dc) = DVar z (Easy.lift3 df da db dc)
    where (z, df) = f a b c

zip :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip (DVar a da) (DVar b db) = DVar (a, b) (Linear.zip da db)
