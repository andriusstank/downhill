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
    GradOf,
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, -- zip
    --fstBVar

)
where

import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, sparseNode, SparseVector (SparseVector), zeroE')
import Prelude (Monad(return), Num, IO, ($), (=<<), Int, undefined, id, (.), Maybe (Just, Nothing))
import Affine (AffineFunc(AffineFunc))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (ProdVector (identityBuilder), BasicVector(..), BackFun (BackFun), FullVector, flipFunc1)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV))
import ExprWalker ()
import Graph (SomeGraph(SomeGraph), evalGraph)
import Data.Coerce (coerce, Coercible)
import OpenGraph (runRecoverSharing7, OpenGraph)
import Data.Kind (Type)

type family GradOf v :: Type

type instance GradOf (u, v) = (GradOf u, GradOf v)

type BVar v a = AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))

type BVarS a = BVar a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: () => a -> BVar a a
constant x = AffineFunc x zeroE'

var :: b -> BVar b b
var x = AffineFunc x anyVar

backprop'' :: forall da dz. BasicVector da => SomeSharedExprWithMap BackFun da dz -> dz -> da
backprop'' m dv = case m of
    NodeMap.SomeSharedExprWithMap smap expr -> evalGraph dx' dv
        where x' = Graph.Graph smap expr -- :: Graph.ForwardGraph s a da v dv
              dx' = Graph.flipGraph flipFunc1 x' -- :: Graph.BackwardGraph s' a da v dv

backprop' :: forall da dv. (BasicVector da, FullVector dv) => AnyExpr BackFun da dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    g <- runRecoverSharing7 dy -- :: IO (NodeMap.SomeSharedExprWithMap BackFun da dv)
    return (backprop'' (cvtmap g) dv)

backprop :: forall b a. (FullVector (GradOf b), BasicVector (GradOf a)) => BVar b a -> GradOf b -> GradOf a
backprop (AffineFunc _y0 y) dv = case y of
    x -> backprop' @(GradOf a) @(GradOf b) x dv

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar b a -> GradOf a
backpropS x = backprop @b @a x 1

fst :: forall b1 b2 a. ProdVector (GradOf b1) => BVar (b1, b2) a -> BVar b1 a
fst (AffineFunc (b1, _) (AnyExpr dv)) = AffineFunc b1 (sparseNode node)
    where f :: BackFun (GradOf (b1, b2)) (SparseVector (GradOf b1))
          f = BackFun (\(SparseVector x) -> (Just x, Nothing))
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1))
          node = ExprSum (dv f)

snd :: forall b1 b2 a. ProdVector (GradOf b2) => BVar (b1, b2) a -> BVar b2 a
snd (AffineFunc (_, b2) (AnyExpr dv)) = AffineFunc b2 (sparseNode node)
    where f :: BackFun (GradOf (b1, b2)) (SparseVector (GradOf b2))
          f = BackFun (\(SparseVector x) -> (Nothing, Just x))
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

