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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Diff
(
    HasGrad(..),
    BVar(..), BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip,
    easyLift2
)
where
import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, sparseNode, SparseVector (SparseVector, unSparseVector), zeroE', zeroE)
import Prelude hiding (fst, snd, zip)
import qualified Prelude
import Affine (AffineFunc(AffineFunc))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (BasicVector(..), BackFun (BackFun, unBackFun), FullVector (identityBuilder), flipBackFun, maybeToMonoid)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(..), Scalar, VectorSpace(..))
import ExprWalker ()
import Graph (SomeGraph(SomeGraph), evalGraph)
import Data.Coerce (coerce, Coercible)
import OpenGraph (runRecoverSharing7, OpenGraph, runRecoverSharing5)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Reflection (Reifies(reflect), reify)
import Data.Proxy (Proxy(Proxy))
import Data.Singletons (type (~>), Apply)

-- add `Scalar v ~ Scalar (GradOf v)` or not?
class (FullVector (GradOf v), Scalar (GradOf v) ~ GradOf (Scalar v)) => HasGrad v where
    type GradOf v :: Type
    evalGrad :: GradOf v -> v -> GradOf (Scalar v)

type family GradBuilder v where
    GradBuilder v = VecBuilder (GradOf v)

type family SparseGrad v where
    SparseGrad v = SparseVector (GradOf v)

instance HasGrad Float where
    type GradOf Float = Float
    evalGrad = (*)

instance HasGrad Double where
    type GradOf Double = Double
    evalGrad = (*)

instance (Scalar u ~ Scalar v, AdditiveGroup (GradOf (Scalar v)), HasGrad u, HasGrad v) => HasGrad (u, v) where
    type GradOf (u, v) = (GradOf u, GradOf v)
    evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y


newtype BExpr a vb = BExpr (forall x. (x->vb) -> [Term BackFun a x])

realBExpr :: forall a v. Expr BackFun a v -> BExpr a (VecBuilder v)
realBExpr node = BExpr g
    where g :: (x -> VecBuilder v) -> [Term BackFun a x]
          g f = [Term (BackFun f) node]

beToAny :: forall a v. BExpr a (VecBuilder v) -> AnyExpr a v
beToAny (BExpr f) = AnyExpr f

data BackGrad a :: Type ~> Type
type instance Apply (BackGrad a) v = AnyExpr (GradOf a) (GradOf v)

--data BackGrad a v = BackGrad (forall x. (x -> GradBuilder v) -> [Term BackFun (GradOf a) x])

newtype BVar a v = BVar (AffineFunc (BackGrad a) v)

deriving via (AffineFunc (BackGrad a) v) instance (Num v, HasGrad v, Scalar v ~ v, GradOf v ~ v) => Num (BVar a v)
deriving via (AffineFunc (BackGrad a) v) instance (Fractional v, HasGrad v, Scalar v ~ v, GradOf v ~ v) => Fractional (BVar a v)
deriving via (AffineFunc (BackGrad a) v) instance (Floating v, HasGrad v, Scalar v ~ v, GradOf v ~ v) => Floating (BVar a v)
deriving via (AffineFunc (BackGrad a) v) instance (AdditiveGroup v, FullVector (GradOf v)) => AdditiveGroup (BVar a v)

type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue (BVar (AffineFunc y0 _)) = y0

zeroBe :: BExpr a vb
zeroBe = BExpr (const [])

anyBVar :: BExpr a (VecBuilder a)
anyBVar = BExpr (\f -> [Term (BackFun f) ExprVar])

constant :: a -> BVar a a
constant x = BVar (AffineFunc x zeroE')

var :: b -> BVar b b
var x = BVar (AffineFunc x anyVar)

backpropNodeMap :: forall a z. BasicVector a => SomeSharedExprWithMap BackFun a z -> z -> a
backpropNodeMap m dv = case m of
    NodeMap.SomeSharedExprWithMap smap expr -> evalGraph fwdGraph dv
        where backGraph = Graph.Graph smap expr
              fwdGraph = Graph.flipGraph flipBackFun backGraph

bexprEdges :: FullVector v => BExpr a (VecBuilder v) -> [Term BackFun a v]
bexprEdges (BExpr f) = f identityBuilder

backpropExpr :: forall a v. (BasicVector a, FullVector v) => AnyExpr a v -> v -> a
backpropExpr f dv = unsafePerformIO $ do
    g <- runRecoverSharing7 f
    return (backpropNodeMap (cvtmap g) dv)

backprop :: forall b a. (FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf b -> GradOf a
backprop (BVar (AffineFunc _y0 x)) = backpropExpr @(GradOf a) @(GradOf b) x

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop @b @a x 1

intoFst :: Monoid x => (SparseVector v -> Maybe (VecBuilder v, x))
intoFst (SparseVector dx) = Just (dx, mempty)

fst :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b1
fst = liftFun1 @(SparseGrad b1) (go . Prelude.fst)
    where go :: b1 -> (b1, SparseGrad b1 -> GradBuilder (b1, b2))
          go x = (x, intoFst)

snd :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b2
snd (BVar (AffineFunc (_, b2) (AnyExpr dv))) = BVar (AffineFunc b2 (castNode node))
    where f :: SparseVector (GradOf b2) -> GradBuilder (b1, b2)
          f (SparseVector x) = Just (mempty, x)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

data DFunc1 a b = forall x. DFunc1 (a -> (b, VecBuilder b -> x, x -> VecBuilder a))
data DFunc2 a b c = forall x. (BasicVector x, VecBuilder x ~ VecBuilder (GradOf c)) => DFunc2 (a -> b -> (c, x -> VecBuilder (GradOf a), x -> VecBuilder (GradOf b)))

liftDenseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, GradOf b -> GradBuilder c)) -> BVar a c -> BVar a b
liftDenseFun1 go = liftSparseFun1 go'
    where go' x = let (y, dy) = go x in (y, dy . sumBuilder')

liftSparseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, VecBuilder (GradOf b) -> VecBuilder (GradOf c))) -> BVar a c -> BVar a b
liftSparseFun1 go (BVar (AffineFunc v0 (AnyExpr dv))) = BVar (AffineFunc y0 (castNode node))
    where f :: SparseVector (GradOf b) -> GradBuilder c
          f = \(SparseVector x) -> goo x
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b))
          node = ExprSum (dv f)
          (y0, goo) = go v0

liftFun1
    :: forall x r a z. (BasicVector (GradOf z), BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> (z, x -> GradBuilder a))
    -> BVar r a -> BVar r z
liftFun1 dfun (BVar (AffineFunc a0 (AnyExpr da))) = BVar (AffineFunc z0 (castNode node))
    where (z0, fa) = dfun a0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa)

-- x -> (GradBuilder a, GradBuilder b) is bad, because it misleadinhly suggests the function will only be called once
liftFunX2
    :: forall x r a b z. (BasicVector (GradOf z), BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> b -> (z, x -> GradBuilder a, x -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r z
liftFunX2 dfun (BVar (AffineFunc a0 (AnyExpr da))) (BVar (AffineFunc b0 (AnyExpr db))) = BVar (AffineFunc z0 (castNode node))
    where (z0, fa, fb) = dfun a0 b0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa ++ db fb)

data BuilderPair s a b z = BuilderPair (VecBuilder a) (VecBuilder b)

newtype Fun2 a b z = Fun2 (z -> (VecBuilder a, VecBuilder b))

instance (Reifies s (Fun2 a b z), BasicVector z) => BasicVector (BuilderPair s a b z) where
    type VecBuilder (BuilderPair s a b z) = VecBuilder z
    sumBuilder' zbs = wrap (f z)
        where z = sumBuilder' zbs :: z
              wrap (a, b) = BuilderPair a b
              Fun2 f = reflect (Proxy :: Proxy s):: Fun2 a b z

builderPairFst :: BuilderPair s a b z -> VecBuilder a
builderPairFst (BuilderPair x _) = x

builderPairSnd :: BuilderPair s a b z -> VecBuilder b
builderPairSnd (BuilderPair _ y) = y


builderPairExpr' :: forall r a b z. (BasicVector z) => Fun2 a b z -> (AnyExpr r a, AnyExpr r b) -> AnyExpr r z
builderPairExpr' f (t1, t2) = reify f go
    where go :: forall s. Reifies s (Fun2 a b z) => Proxy s -> AnyExpr r z
          go _ = castNode (ExprSum (mkT1 t1 ++ mkT2 t2) :: Expr BackFun r (BuilderPair s a b z))
          mkT1 :: forall s. AnyExpr r a -> [Term BackFun r (BuilderPair s a b z)]
          mkT1 (AnyExpr g) = g builderPairFst
          mkT2 :: forall s. AnyExpr r b -> [Term BackFun r (BuilderPair s a b z)]
          mkT2 (AnyExpr g) = g builderPairSnd

easyLift2 :: forall r a b z. BasicVector z => (z -> (VecBuilder a, VecBuilder b)) -> AnyExpr r a -> AnyExpr r b -> AnyExpr r z
easyLift2 f a b = builderPairExpr' (Fun2 f) (a, b)

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
liftFun3 dfun (BVar (AffineFunc a0 (AnyExpr da))) (BVar (AffineFunc b0 (AnyExpr db))) (BVar (AffineFunc c0 (AnyExpr dc))) = BVar (AffineFunc z0 (castNode node))
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

instance (VectorSpace v, VectorSpace (GradOf v), FullVector (GradOf (Scalar v)), GradOf (Scalar v) ~ Scalar v, FullVector (GradOf v), HasGrad v) => VectorSpace (BVar a v) where
    type Scalar (BVar a v) = BVar a (Scalar v)
    BVar (AffineFunc a (AnyExpr da)) *^ BVar (AffineFunc v (AnyExpr dv)) = BVar (AffineFunc (a *^ v) (realExpr node))
        where node :: Expr BackFun (GradOf a) (GradOf v)
              node = ExprSum (term1 ++ term2)
                where term1 :: [Term BackFun (GradOf a) (GradOf v)]
                      term1  = da (\v' -> identityBuilder (evalGrad v' v))
                      term2 :: [Term BackFun (GradOf a) (GradOf v)]
                      term2 = dv (\v' -> identityBuilder (a *^ v'))
