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
module Diff
(
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip
)
where

import Expr(Expr5(Expr5), LinearFunc5, Endpoint' (..), Edge'(..))
import Prelude (Monad(return), Num, IO, ($))
import Affine (AffineFunc(AffineFunc))
import Tensor (Bilinear(..), Vec(..))
import NodeMap (runRecoverSharing5)

import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (ProdVector, BasicVector(..), fstF1, sndF1, intoFst, intoSnd, BackFunc, FullVector)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV))

type BVar b da dv = AffineFunc b (LinearFunc5 BackFunc da dv)

type BVarS a = BVar a a a

--instance BasicVector (Endpoint (Expr5 BackFunc da) da dv) where
--instance ProdVector (Endpoint (Expr5 BackFunc da) da dv) where
--instance FullVector (Endpoint (Expr5 BackFunc da) da dv) where
--instance VectorSpace (Endpoint (Expr5 BackFunc da) da dv) where
--    type Scalar (Endpoint (Expr5 BackFunc da) da dv) = Double
    
bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: FullVector dv => b -> BVar b da dv
constant x = AffineFunc x zeroV

var :: b -> BVar b dv dv
var x = AffineFunc x SourceNode'

backprop' :: forall da dv. BasicVector da => Expr5 BackFunc da dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    NodeMap.SomeSharedExprWithMap smap expr <- runRecoverSharing5 dy :: IO (NodeMap.SomeSharedExprWithMap BackFunc da dv)
    let x' = Graph.NonTrivialGraph (Graph.Graph smap expr) -- :: Graph.ForwardGraph s a da v dv
        dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv
    return (unVec (dx' ✕ Vec dv))

backprop :: forall b da dv. BasicVector da => BVar b da dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    SourceNode' -> dv
    InnerNode' x -> backprop' x dv

backpropS :: (BasicVector da, Num dv) => BVar b da dv -> da
backpropS x = backprop x 1

liftFunc1 
  :: forall u v da dv du. 
     BasicVector dv
  => (u -> (v, BackFunc du dv))
  -> AffineFunc u (LinearFunc5 BackFunc da du)
  -> AffineFunc v (LinearFunc5 BackFunc da dv)
liftFunc1 f (AffineFunc x0 dx) = AffineFunc y0 expr
    where term :: Edge' BackFunc da dv
          term = Edge' df dx
          expr :: Endpoint' BackFunc da dv
          expr = InnerNode' (Expr5 [term])
          (y0, df) = f x0


fst :: forall b1 b2 da du dv. ProdVector du => BVar (b1, b2) da (du, dv) -> BVar b1 da du
fst = liftFunc1 go
    where go (x, _) = (x, fstF1)

snd :: forall b1 b2 da du dv. ProdVector dv => BVar (b1, b2) da (du, dv) -> BVar b2 da dv
snd = liftFunc1 go
    where go (_, x) = (x, sndF1)

zipA
  :: forall da du dv. (ProdVector du, ProdVector dv)
  => LinearFunc5 BackFunc da du
  -> LinearFunc5 BackFunc da dv
  -> LinearFunc5 BackFunc da (du, dv)
zipA x y = InnerNode' (Expr5 [Edge' intoFst x, Edge' intoSnd y])

zip :: (ProdVector du, ProdVector dv) => BVar b da du -> BVar c da dv -> BVar (b, c) da (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
