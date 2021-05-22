{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Diff
(
    HasGrad(..),
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip,
    easyLift2
)
where
import Downhill.Linear.Expr(Expr(ExprSum, ExprVar), Term(..), SparseVector (SparseVector, unSparseVector), zeroExpr, BasicVector (..), maybeToMonoid, BackFun(..), flipBackFun)
import Prelude hiding (fst, snd, zip)
import qualified Prelude
import Affine (DVar(DVar))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (FullVector (identityBuilder, negateBuilder, scaleBuilder))
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(..), Scalar, VectorSpace(..))
import ExprWalker ()
import Graph (SomeGraph(SomeGraph), evalGraph)
import Data.Coerce (coerce, Coercible)
import OpenGraph (OpenGraph, runRecoverSharing5)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Reflection (Reifies(reflect), reify)
import Data.Proxy (Proxy(Proxy))
import Data.Singletons (type (~>), Apply, TyCon1)
import Downhill.Linear.BackGrad(BackGrad(..), HasGrad (GradOf, evalGrad), GradBuilder, SparseGrad, castGradNode)

type BVar a v = DVar (BackGrad a) v


type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue (DVar y0 _) = y0

constant :: a -> BVar a a
constant x = DVar x (BackGrad (const []))

var :: b -> BVar b b
var x = DVar x (BackGrad (\f -> [Term (BackFun f) ExprVar]))

backpropNodeMap :: forall a z. BasicVector a => SomeSharedExprWithMap BackFun a z -> z -> a
backpropNodeMap m dv = case m of
    NodeMap.SomeSharedExprWithMap smap expr -> evalGraph fwdGraph dv
        where backGraph = Graph.Graph smap expr
              fwdGraph = Graph.flipGraph flipBackFun backGraph

backpropExpr :: forall a v. (BasicVector (GradOf a), FullVector (GradOf v)) => BackGrad a v -> GradOf v -> GradOf a
backpropExpr (BackGrad f) dv = unsafePerformIO $ do
    g <- runRecoverSharing5 (f identityBuilder)
    return (backpropNodeMap (cvtmap g) dv)

backprop :: forall b a. (FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf b -> GradOf a
backprop (DVar _y0 x) = backpropExpr x

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop @b @a x 1

intoFst :: Monoid x => (SparseVector v -> Maybe (VecBuilder v, x))
intoFst (SparseVector dx) = Just (dx, mempty)

fst :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b1
fst = liftFun1 @(SparseGrad b1) (go . Prelude.fst)
    where go :: b1 -> (b1, SparseGrad b1 -> GradBuilder (b1, b2))
          go x = (x, intoFst)

snd :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b2
snd (DVar (_, b2) (BackGrad dv)) = DVar b2 (castGradNode node)
    where f :: SparseVector (GradOf b2) -> GradBuilder (b1, b2)
          f (SparseVector x) = Just (mempty, x)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

data DFunc1 a b = forall x. DFunc1 (a -> (b, VecBuilder b -> x, x -> VecBuilder a))
data DFunc2 a b c = forall x. (BasicVector x, VecBuilder x ~ VecBuilder (GradOf c)) => DFunc2 (a -> b -> (c, x -> VecBuilder (GradOf a), x -> VecBuilder (GradOf b)))

liftDenseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, GradOf b -> GradBuilder c)) -> BVar a c -> BVar a b
liftDenseFun1 go = liftSparseFun1 go'
    where go' x = let (y, dy) = go x in (y, dy . sumBuilder)

liftSparseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, VecBuilder (GradOf b) -> VecBuilder (GradOf c))) -> BVar a c -> BVar a b
liftSparseFun1 go (DVar v0 (BackGrad dv)) = DVar y0 (castGradNode node)
    where f :: SparseVector (GradOf b) -> GradBuilder c
          f = \(SparseVector x) -> goo x
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b))
          node = ExprSum (dv f)
          (y0, goo) = go v0

liftFun1
    :: forall x r a z. (BasicVector (GradOf z), BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> (z, x -> GradBuilder a))
    -> BVar r a -> BVar r z
liftFun1 dfun (DVar a0 (BackGrad da)) = DVar z0 (castGradNode node)
    where (z0, fa) = dfun a0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa)

-- x -> (GradBuilder a, GradBuilder b) is bad, because it misleadinhly suggests the function will only be called once
liftFunX2
    :: forall x r a b z. (BasicVector (GradOf z), BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> b -> (z, x -> GradBuilder a, x -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r z
liftFunX2 dfun (DVar a0 (BackGrad da)) (DVar b0 (BackGrad db)) = DVar z0 (castGradNode node)
    where (z0, fa, fb) = dfun a0 b0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa ++ db fb)

data BuilderPair s a b z = BuilderPair (GradBuilder a) (GradBuilder b)

newtype Fun2 a b z = Fun2 (GradOf z -> (GradBuilder a, GradBuilder b))

instance (Reifies s (Fun2 a b z), BasicVector (GradOf z)) => BasicVector (BuilderPair s a b z) where
    type VecBuilder (BuilderPair s a b z) = GradBuilder z
    sumBuilder zbs = wrap (f z)
        where z = sumBuilder zbs :: GradOf z
              wrap (a, b) = BuilderPair a b
              Fun2 f = reflect (Proxy :: Proxy s):: Fun2 a b z

builderPairFst :: BuilderPair s a b z -> GradBuilder a
builderPairFst (BuilderPair x _) = x

builderPairSnd :: BuilderPair s a b z -> GradBuilder b
builderPairSnd (BuilderPair _ y) = y

builderPairExpr' :: forall r a b z. (BasicVector (GradOf z)) => Fun2 a b z -> (BackGrad r a, BackGrad r b) -> BackGrad r z
builderPairExpr' f (t1, t2) = reify f go
    where go :: forall s. Reifies s (Fun2 a b z) => Proxy s -> BackGrad r z
          go _ = castGradNode (ExprSum (mkT1 t1 ++ mkT2 t2) :: Expr BackFun (GradOf r) (BuilderPair s a b z))
          mkT1 :: forall s. BackGrad r a -> [Term BackFun (GradOf r) (BuilderPair s a b z)]
          mkT1 (BackGrad g) = g builderPairFst
          mkT2 :: forall s. BackGrad r b -> [Term BackFun (GradOf r) (BuilderPair s a b z)]
          mkT2 (BackGrad g) = g builderPairSnd

easyLift2 :: forall r a b z. BasicVector (GradOf z) => (GradOf z -> (GradBuilder a, GradBuilder b)) -> BackGrad r a -> BackGrad r b -> BackGrad r z
easyLift2 f a b = builderPairExpr' (Fun2 f) (a, b)

easyLift2'
    :: BasicVector (GradOf z)
    => (a -> b -> (z, GradOf z -> (GradBuilder a, GradBuilder b)))
    -> BVar r a -> BVar r b -> BVar r z
easyLift2' f (DVar a da) (DVar b db) = DVar z (easyLift2 df da db)
    where (z, df) = f a b

liftDenseFun2
    :: forall r a b z. (BasicVector (GradOf z))
    => (a -> b -> (z, GradOf z -> GradBuilder a,GradOf z -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r z
liftDenseFun2 = liftFunX2 @(GradOf z)

liftSparseFun2
    :: forall r a b z. (BasicVector (GradOf z))
    => (a -> b -> (z, GradBuilder z -> GradBuilder a, GradBuilder z -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r z
liftSparseFun2 f = liftFunX2 @(SparseGrad z) f'
    where f' a b = (z, da . unSparseVector, db . unSparseVector)
            where (z, da, db) = f a b

liftFun3
    :: forall x r a b c z. (BasicVector (GradOf c), BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> b -> c -> (z, x -> GradBuilder a, x -> GradBuilder b, x -> GradBuilder c))
    -> BVar r a -> BVar r b -> BVar r c -> BVar r z
liftFun3 dfun (DVar a0 (BackGrad da)) (DVar b0 (BackGrad db)) (DVar c0 (BackGrad dc)) = DVar z0 (castGradNode node)
    where (z0, fa, fb, fc) = dfun a0 b0 c0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa ++ db fb ++ dc fc)

snd' :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b2
snd' = liftSparseFun1 @(b1, b2) @b2 d_snd
    where d_snd (_, y) = (y, \dy -> Just (mempty, dy))

zip :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip = liftSparseFun2 go
    where go :: b1 -> b2 -> ((b1, b2), Maybe (VecBuilder (GradOf b1), VecBuilder (GradOf b2)) -> VecBuilder (GradOf b1), Maybe (VecBuilder (GradOf b1), VecBuilder (GradOf b2)) -> VecBuilder (GradOf b2))
          go b1 b2 = ((b1, b2), Prelude.fst . maybeToMonoid, Prelude.snd . maybeToMonoid)
