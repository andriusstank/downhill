{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Diff
(
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    --fst, snd, zip

)
where

import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, SparseVector (SparseVector))
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

type BVar b a v = AffineFunc b (AnyExpr BackFun a v)

type BVarS a = BVar a a a

--instance BasicVector (Endpoint (Expr BackFun da) da dv) where
--instance ProdVector (Endpoint (Expr BackFun da) da dv) where
--instance FullVector (Endpoint (Expr BackFun da) da dv) where
--instance VectorSpace (Endpoint (Expr BackFun da) da dv) where
--    type Scalar (Endpoint (Expr BackFun da) da dv) = Double
    
bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: FullVector dv => b -> BVar b da dv
constant x = AffineFunc x zeroV

var :: b -> BVar b dv dv
var x = AffineFunc x anyVar

backprop'' :: forall g da dz. BasicVector da => SomeSharedExprWithMap BackFun da dz -> dz -> da
backprop'' m dv = case m of
    NodeMap.TrivialSharedExprWithMap -> dv
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

fstF1' :: forall u v. ProdVector u => BackFun (u, v) (SparseVector u)
fstF1' = BackFun back
    where back (SparseVector x) = (Just x, Nothing)

fst :: forall b1 b2 a u v. ProdVector u => BVar (b1, b2) a (u, v) -> BVar b1 a u
fst (AffineFunc (y0, _y1) (AnyExpr dy0)) = AffineFunc y0 (castNode node)
    where node :: Expr BackFun a (SparseVector u)
          node = ExprSum [dy0 fstF1']


{-
liftFunc1 
  :: forall u v da dv du. 
     BasicVector dv
  => (u -> (v, BackFun du dv))
  -> AffineFunc u (AnyExpr BackFun da du)
  -> AffineFunc v (AnyExpr BackFun da dv)
liftFunc1 f (AffineFunc x0 dx) = AffineFunc y0 expr
    where term :: Term BackFun da dv
          term = Term df dx
          expr :: Expr BackFun da dv
          expr = ExprSum [term]
          (y0, df) = f x0


snd :: forall b1 b2 da du dv. ProdVector dv => BVar (b1, b2) da (du, dv) -> BVar b2 da dv
snd = liftFunc1 go
    where go (_, x) = (x, sndF1)
zipA
  :: forall da du dv. (ProdVector du, ProdVector dv)
  => Expr BackFun da du
  -> Expr BackFun da dv
  -> Expr BackFun da (du, dv)
zipA x y = ExprSum [Term intoFst x, Term intoSnd y]

zip :: (ProdVector du, ProdVector dv) => BVar b da du -> BVar c da dv -> BVar (b, c) da (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
-}
