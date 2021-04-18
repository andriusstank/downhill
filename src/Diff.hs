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
    fst, snd, zip

)
where

import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr))
import Prelude (Monad(return), Num, IO, ($), (=<<), Int, undefined, id, (.))
import Affine (AffineFunc(AffineFunc))
import Tensor (Bilinear(..), Vec(..))
import NodeMap (cvtmap, SomeSharedExprWithMap)

import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (ProdVector, BasicVector(..), fstF1, sndF1, intoFst, intoSnd, BackFunc (BackFunc), FullVector, Transpose)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV))
import ExprWalker ()
import Graph (SomeGraph(SomeGraph))
import Data.Coerce (coerce, Coercible)
import OpenGraph (runRecoverSharing4, OpenGraph)

type BVar b da dv = AffineFunc b (Expr BackFunc da dv)

type BVarS a = BVar a a a

--instance BasicVector (Endpoint (Expr BackFunc da) da dv) where
--instance ProdVector (Endpoint (Expr BackFunc da) da dv) where
--instance FullVector (Endpoint (Expr BackFunc da) da dv) where
--instance VectorSpace (Endpoint (Expr BackFunc da) da dv) where
--    type Scalar (Endpoint (Expr BackFunc da) da dv) = Double
    
bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: FullVector dv => b -> BVar b da dv
constant x = AffineFunc x zeroV

var :: b -> BVar b dv dv
var x = AffineFunc x ExprVar

runRecoverSharing6 :: Expr e da dz -> IO (OpenGraph e da dz)
runRecoverSharing6 = runRecoverSharing4

backprop'' :: forall g da dz. (BasicVector da, Transpose BackFunc g) => SomeSharedExprWithMap BackFunc da dz -> dz -> da
backprop'' m dv = case m of
    NodeMap.TrivialSharedExprWithMap -> dv
    NodeMap.SomeSharedExprWithMap smap expr -> unVec (dx' ✕ Vec dv)
        where x' = Graph.NonTrivialGraph (Graph.Graph smap expr) -- :: Graph.ForwardGraph s a da v dv
              dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv

backprop' :: forall da dv. (BasicVector da, FullVector dv) => Expr BackFunc da dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    g <- runRecoverSharing6 dy -- :: IO (NodeMap.SomeSharedExprWithMap BackFunc da dv)
    return (backprop'' (cvtmap g) dv)

backprop :: forall b da dv. (BasicVector da, FullVector dv) => BVar b da dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    x -> backprop' x dv

backpropS :: (BasicVector da, FullVector dv, Num dv) => BVar b da dv -> da
backpropS x = backprop x 1

liftFunc1 
  :: forall u v da dv du. 
     BasicVector dv
  => (u -> (v, BackFunc du dv))
  -> AffineFunc u (Expr BackFunc da du)
  -> AffineFunc v (Expr BackFunc da dv)
liftFunc1 f (AffineFunc x0 dx) = AffineFunc y0 expr
    where term :: Term BackFunc da dv
          term = Term df dx
          expr :: Expr BackFunc da dv
          expr = ExprSum [term]
          (y0, df) = f x0


fst :: forall b1 b2 da du dv. ProdVector du => BVar (b1, b2) da (du, dv) -> BVar b1 da du
fst = liftFunc1 go
    where go (x, _) = (x, fstF1)

snd :: forall b1 b2 da du dv. ProdVector dv => BVar (b1, b2) da (du, dv) -> BVar b2 da dv
snd = liftFunc1 go
    where go (_, x) = (x, sndF1)

zipA
  :: forall da du dv. (ProdVector du, ProdVector dv)
  => Expr BackFunc da du
  -> Expr BackFunc da dv
  -> Expr BackFunc da (du, dv)
zipA x y = ExprSum [Term intoFst x, Term intoSnd y]

zip :: (ProdVector du, ProdVector dv) => BVar b da du -> BVar c da dv -> BVar (b, c) da (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
