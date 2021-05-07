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
module Diff
(
    HasGrad(..),
    BVar(..), BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip,
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

-- add `Scalar v ~ Scalar (GradOf v)` or not?
class (FullVector (GradOf v), Scalar (GradOf v) ~ Scalar v) => HasGrad v where
    type GradOf v :: Type
    evalGrad :: GradOf v -> v -> Scalar v

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

instance (Scalar u ~ Scalar v, AdditiveGroup (Scalar v), HasGrad u, HasGrad v) => HasGrad (u, v) where
    type GradOf (u, v) = (GradOf u, GradOf v)
    evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y


newtype BExpr a vb = BExpr (forall x. (x->vb) -> [Term BackFun a x])

realBExpr :: forall a v. Expr BackFun a v -> BExpr a (VecBuilder v)
realBExpr node = BExpr g
    where g :: (x -> VecBuilder v) -> [Term BackFun a x]
          g f = [Term (BackFun f) node]

beToAny :: forall a v. BExpr a (VecBuilder v) -> AnyExpr a v
beToAny (BExpr f) = AnyExpr f

newtype BVar a v = BVar (AffineFunc v (AnyExpr (GradOf a) (GradOf v)))

deriving via (AffineFunc v (AnyExpr (GradOf a) (GradOf v))) instance (Num v, HasGrad v, Scalar v ~ v) => Num (BVar a v)
deriving via (AffineFunc v (AnyExpr (GradOf a) (GradOf v))) instance (Fractional v, HasGrad v, Scalar v ~ v) => Fractional (BVar a v)
deriving via (AffineFunc v (AnyExpr (GradOf a) (GradOf v))) instance (Floating v, HasGrad v, Scalar v ~ v) => Floating (BVar a v)

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

fst :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a (b1, b2) -> BVar a b1
fst (BVar (AffineFunc (b1, _) (AnyExpr dv))) = BVar (AffineFunc b1 (castNode node))
    where f :: SparseVector (GradOf b1) -> GradBuilder (b1, b2)
          f (SparseVector x) = Just (x, mempty)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1))
          node = ExprSum (dv f)

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

liftFun2
    :: forall x r a b z. (BasicVector (GradOf z), BasicVector x, VecBuilder x ~ GradBuilder z)
    => (a -> b -> (z, x -> GradBuilder a, x -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r z
liftFun2 dfun (BVar (AffineFunc a0 (AnyExpr da))) (BVar (AffineFunc b0 (AnyExpr db))) = BVar (AffineFunc z0 (castNode node))
    where (z0, fa, fb) = dfun a0 b0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa ++ db fb)


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
zip (BVar (AffineFunc b1 (AnyExpr db1))) (BVar (AffineFunc b2 (AnyExpr db2))) = BVar (AffineFunc (b1, b2) (castNode node))
    where f1' :: SparseGrad (b1, b2) -> GradBuilder b1
          f1' = Prelude.fst . maybeToMonoid . unSparseVector
          f2' :: SparseGrad (b1, b2) -> GradBuilder b2
          f2' = Prelude.snd . maybeToMonoid . unSparseVector
          node :: Expr BackFun (GradOf a) (SparseGrad (b1, b2))
          node = ExprSum (db1 f1' ++ db2 f2')

zip' :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip' = liftFun2 @(SparseGrad (b1, b2)) go
    where go x y = ((x, y), getFst, getSnd)
            where getFst :: SparseGrad (b1, b2) -> GradBuilder b1
                  getFst = Prelude.fst . maybeToMonoid . unSparseVector
                  getSnd :: SparseGrad (b1, b2) -> GradBuilder b2
                  getSnd = Prelude.snd . maybeToMonoid . unSparseVector
