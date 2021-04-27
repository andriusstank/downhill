{-# LANGUAGE TypeFamilies #-}
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
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    --fst, snd, zip
    --fstBVar

)
where

import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, sparseNode, SparseVector (SparseVector))
import Prelude (Monad(return), Num, IO, ($), (=<<), Int, undefined, id, (.), Maybe (Just, Nothing))
import Affine (AffineFunc(AffineFunc))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (ProdVector (identityBuilder), BasicVector(..), fstF1, sndF1, intoFst, intoSnd, BackFun (BackFun), FullVector, flipFunc1)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV))
import ExprWalker ()
import Graph (SomeGraph(SomeGraph), evalGraph)
import Data.Coerce (coerce, Coercible)
import OpenGraph (runRecoverSharing7, OpenGraph)
import Data.Kind (Type)

type family GradOf v :: Type

type BVar b a v = AffineFunc b (AnyExpr BackFun a v)
--type BVar v a = AffineFunc v (AnyExpr BackFun a (Expr BackFun a (GradOf v)))

type BVarS a = BVar a a a
    
bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: FullVector dv => b -> BVar b da dv
constant x = AffineFunc x zeroV

var :: b -> BVar b dv dv
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

backprop :: forall b da dv. (BasicVector da, FullVector dv) => BVar b da dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    x -> backprop' x dv

backpropS :: (BasicVector da, FullVector dv, Num dv) => BVar b da dv -> da
backpropS x = backprop x 1

--fstBVar :: BVar (b1, b2) a _v -> BVar b1 a v'
--fstBVar (AffineFunc (b1, _) dv) = AffineFunc b1 (sparseNode (ExprSum [Term fstF1 dv]))
