{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Diff
(
    HasGrad(..),
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip
)
where

import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, sparseNode, SparseVector (SparseVector), zeroE')
import Prelude (Monad(return), Num, IO, ($), (=<<), Int, undefined, id, (.), Maybe (Just, Nothing), Semigroup ((<>)), Monoid (mempty))
import Affine (AffineFunc(AffineFunc))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (BasicVector(..), BackFun (BackFun), FullVector, flipBackFun)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV))
import ExprWalker ()
import Graph (SomeGraph(SomeGraph), evalGraph)
import Data.Coerce (coerce, Coercible)
import OpenGraph (runRecoverSharing7, OpenGraph)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)

class BasicVector (GradOf v) => HasGrad v where
    type GradOf v :: Type

instance (HasGrad u, HasGrad v) => HasGrad (u, v) where
    type GradOf (u, v) = (GradOf u, GradOf v)

type BVar v a = AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))

type BVarS a = BVar a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: a -> BVar a a
constant x = AffineFunc x zeroE'

var :: b -> BVar b b
var x = AffineFunc x anyVar

backpropNodeMap :: forall a z. BasicVector a => SomeSharedExprWithMap BackFun a z -> z -> a
backpropNodeMap m dv = case m of
    NodeMap.SomeSharedExprWithMap smap expr -> evalGraph fwdGraph dv
        where backGraph = Graph.Graph smap expr
              fwdGraph = Graph.flipGraph flipBackFun backGraph

backpropExpr :: forall da dv. (BasicVector da, FullVector dv) => AnyExpr BackFun da dv -> dv -> da
backpropExpr dy dv = unsafePerformIO $ do
    g <- runRecoverSharing7 dy
    return (backpropNodeMap (cvtmap g) dv)

backprop :: forall b a. (FullVector (GradOf b), BasicVector (GradOf a)) => BVar b a -> GradOf b -> GradOf a
backprop (AffineFunc _y0 x) = backpropExpr @(GradOf a) @(GradOf b) x

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar b a -> GradOf a
backpropS x = backprop @b @a x 1

fst :: forall b1 b2 a. BasicVector (GradOf b1) => BVar (b1, b2) a -> BVar b1 a
fst (AffineFunc (b1, _) (AnyExpr dv)) = AffineFunc b1 (sparseNode node)
    where f :: BackFun (GradOf (b1, b2)) (SparseVector (GradOf b1))
          f = BackFun (\(SparseVector x) -> (Just x, Nothing))
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1))
          node = ExprSum (dv f)

snd :: forall b1 b2 a. BasicVector (GradOf b2) => BVar (b1, b2) a -> BVar b2 a
snd (AffineFunc (_, b2) (AnyExpr dv)) = AffineFunc b2 (sparseNode node)
    where f :: BackFun (GradOf (b1, b2)) (SparseVector (GradOf b2))
          f = BackFun (\(SparseVector x) -> (Nothing, Just x))
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

zip :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar b1 a -> BVar b2 a -> BVar (b1, b2) a
zip (AffineFunc b1 (AnyExpr db1)) (AffineFunc b2 (AnyExpr db2)) = AffineFunc (b1, b2) (sparseNode node)
    where f1 :: BackFun (GradOf b1) (SparseVector (GradOf (b1, b2)))
          f1 = BackFun (\(SparseVector (x, _y)) -> fromMaybe mempty x)
          f2 :: (SparseVector (GradOf b1, GradOf b2) -> VecBuilder (GradOf b2))
          f2 = \(SparseVector (_x, y)) -> fromMaybe mempty y
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1, GradOf b2))
          node = ExprSum (db1 f1 <> db2 (BackFun f2))
