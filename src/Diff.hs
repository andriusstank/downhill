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
import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, sparseNode, SparseVector (SparseVector, unSparseVector), zeroE')
import Prelude hiding (fst, snd, zip)
import qualified Prelude
import Affine (AffineFunc(AffineFunc))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (BasicVector(..), BackFun (BackFun, unBackFun), FullVector (identityBuilder), flipBackFun, maybeToMonoid)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV, (^+^)), Scalar)
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

anyToBe :: AnyExpr BackFun a v -> BExpr a (VecBuilder v)
anyToBe (AnyExpr f) = BExpr (f . BackFun)

beToAny :: BExpr a (VecBuilder v) -> AnyExpr BackFun a v
beToAny (BExpr f) = AnyExpr (f . unBackFun)

newtype BVar a v = BVar (AffineFunc v (BExpr (GradOf a) (VecBuilder (GradOf v))))

deriving via (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))) instance (Num v, HasGrad v, Scalar v ~ v) => Num (BVar a v)
deriving via (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))) instance (Fractional v, HasGrad v, Scalar v ~ v) => Fractional (BVar a v)
deriving via (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))) instance (Floating v, HasGrad v, Scalar v ~ v) => Floating (BVar a v)

type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue (BVar (AffineFunc y0 _)) = y0

zeroBe :: BExpr a vb
zeroBe = BExpr (const [])

anyBVar :: BExpr a (VecBuilder a)
anyBVar = BExpr (\f -> [Term (BackFun f) ExprVar])

constant :: a -> BVar a a
constant x = BVar (AffineFunc x zeroBe)

var :: b -> BVar b b
var x = BVar (AffineFunc x anyBVar)

backpropNodeMap :: forall a z. BasicVector a => SomeSharedExprWithMap BackFun a z -> z -> a
backpropNodeMap m dv = case m of
    NodeMap.SomeSharedExprWithMap smap expr -> evalGraph fwdGraph dv
        where backGraph = Graph.Graph smap expr
              fwdGraph = Graph.flipGraph flipBackFun backGraph

backpropExpr :: forall a v. (BasicVector a, FullVector v) => BExpr a (VecBuilder v) -> v -> a
backpropExpr be dv = unsafePerformIO $ do
    g <- runRecoverSharing7 (beToAny be)
    --g <- runRecoverSharing5 (dy identityBuilder)
    return (backpropNodeMap (cvtmap g) dv)

backprop :: forall b a. (FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf b -> GradOf a
backprop (BVar (AffineFunc _y0 x)) = backpropExpr @(GradOf a) @(GradOf b) x

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop @b @a x 1

-- TODO: remove after refactoring
type BackFun' u v = v -> VecBuilder u

sparseNode' :: forall a b. Expr BackFun a (SparseVector b) -> BExpr a (VecBuilder b)
sparseNode' = anyToBe . realExpr

fst :: forall b1 b2 a. BasicVector (GradOf b1) => BVar a (b1, b2) -> BVar a b1
fst (BVar (AffineFunc (b1, _) (BExpr dv))) = BVar (AffineFunc b1 (sparseNode' node))
    where f :: BackFun' (GradOf (b1, b2)) (SparseVector (GradOf b1))
          f = \(SparseVector x) -> (Just x, Nothing)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1))
          node = ExprSum (dv f)

snd :: forall b1 b2 a. BasicVector (GradOf b2) => BVar a (b1, b2) -> BVar a b2
snd (BVar (AffineFunc (_, b2) (BExpr dv))) = BVar (AffineFunc b2 (sparseNode' node))
    where f :: BackFun' (GradOf (b1, b2)) (SparseVector (GradOf b2))
          f = \(SparseVector x) -> (Nothing, Just x)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

data DFunc1 a b = forall x. DFunc1 (a -> (b, VecBuilder b -> x, x -> VecBuilder a))
data DFunc2 a b c = forall x. (BasicVector x, VecBuilder x ~ VecBuilder (GradOf c)) => DFunc2 (a -> b -> (c, x -> VecBuilder (GradOf a), x -> VecBuilder (GradOf b)))

liftDenseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, GradOf b -> VecBuilder (GradOf c))) -> BVar a c -> BVar a b
liftDenseFun1 go = liftSparseFun1 go'
    where go' x = let (y, dy) = go x in (y, dy . sumBuilder')

liftSparseFun1 :: forall c b a. BasicVector (GradOf b) => (c -> (b, VecBuilder (GradOf b) -> VecBuilder (GradOf c))) -> BVar a c -> BVar a b
liftSparseFun1 go (BVar (AffineFunc v0 (BExpr dv))) = BVar (AffineFunc y0 (sparseNode' node))
    where f :: BackFun' (GradOf c) (SparseVector (GradOf b))
          f = \(SparseVector x) -> goo x
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b))
          node = ExprSum (dv f)
          (y0, goo) = go v0

liftFun2'
    :: forall x r a b c. (BasicVector (GradOf c), BasicVector x, VecBuilder x ~ GradBuilder c)
    => (a -> b -> (c, x -> GradBuilder a, x -> GradBuilder b))
    -> BVar r a -> BVar r b -> BVar r c
liftFun2' dfun (BVar (AffineFunc a0 (BExpr da))) (BVar (AffineFunc b0 (BExpr db))) = BVar (AffineFunc z0 (anyToBe (realExpr node)))
    where (z0, fa, fb) = dfun a0 b0
          node :: Expr BackFun (GradOf r) x
          node = ExprSum (da fa ++ db fb)

liftFun2 :: BasicVector (GradOf c) => DFunc2 a b c -> BVar r a -> BVar r b -> BVar r c
liftFun2 (DFunc2 f) = liftFun2' f

{-
liftSparseFun2 :: forall c1 c2 b a. (BasicVector (GradOf c1), BasicVector (GradOf c2)) => (c1 -> c2 -> (b, VecBuilder (GradOf b) -> (VecBuilder (GradOf c1), VecBuilder (GradOf c1)))) -> BVar a c1 -> BVar a c2 -> BVar a b
liftSparseFun2 go (BVar (AffineFunc x0 (AnyExpr dx))) (BVar (AffineFunc y0 (AnyExpr dy))) = BVar (AffineFunc z0 (AnyExpr makeEdge))
    where --f :: BackFun (GradOf (c1, c2)) (SparseVector (GradOf b))
          --f = BackFun (\(SparseVector x) -> _goo x)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf c1, GradOf c2))
          node = ExprSum (dx (BackFun getFst) ++ dy (BackFun getSnd))
          getFst :: SparseVector (GradOf c1, GradOf c2) -> VecBuilder (GradOf c1)
          getFst (SparseVector (x, _)) = maybeToMonoid x
          getSnd :: SparseVector (GradOf c1, GradOf c2) -> VecBuilder (GradOf c2)
          getSnd (SparseVector (_, y)) = maybeToMonoid y
          (z0, goo) = go x0 y0
          makeEdge :: forall x. BackFun (GradOf b) x -> [Term BackFun (GradOf a) x]
          makeEdge = _
-}

snd' :: forall b1 b2 a. BasicVector (GradOf b2) => BVar a (b1, b2) -> BVar a b2
snd' = liftSparseFun1 @(b1, b2) @b2 d_snd
    where d_snd (_, y) = (y, \dy -> (Nothing, Just dy))

zip :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip (BVar (AffineFunc b1 (BExpr db1))) (BVar (AffineFunc b2 (BExpr db2))) = BVar (AffineFunc (b1, b2) (sparseNode' node))
    where f1' :: SparseVector (GradOf (b1, b2)) -> VecBuilder (GradOf b1)
          f2' :: SparseVector (GradOf (b1, b2)) -> VecBuilder (GradOf b2)
          f1' (SparseVector (x, _)) = maybeToMonoid x
          f2' (SparseVector (_, y)) = maybeToMonoid y
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1, GradOf b2))
          node = ExprSum (db1 f1' ++ db2 f2')

zip' :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip' = liftFun2' @(SparseGrad (b1, b2)) go
    where go x y = ((x, y), getFst, getSnd)
            where getFst :: SparseGrad (b1, b2) -> GradBuilder b1
                  getFst = maybeToMonoid . Prelude.fst . unSparseVector
                  getSnd :: SparseGrad (b1, b2) -> GradBuilder b2
                  getSnd = maybeToMonoid . Prelude.snd . unSparseVector
