{-# language ScopedTypeVariables #-}

module Diff
(
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS
)
where

import Expr(ExprArg(ArgVar), Expr2(Expr2), Expr3(ExprSum), Term3(Func2))
import Affine (AffineFunc(AffineFunc))
import Data.VectorSpace (AdditiveGroup(zeroV))
import Tensor (TensorProduct((⊗)), AFunction(IndentityFunc))
import NodeMap (runRecoverSharing5)

import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)

type BVar b a da v dv = AffineFunc b (Expr2 a da v dv)

type BVarS a = BVar a a a a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: (AdditiveGroup v, AdditiveGroup dv) => b -> BVar b a da v dv
constant x = AffineFunc x zeroV

var :: (AdditiveGroup v, AdditiveGroup dv) => b -> BVar b v dv v dv
var x = AffineFunc x (Expr2 (ExprSum [Func2 IndentityFunc ArgVar]))

backprop :: forall b a da v dv. AdditiveGroup da => BVar b a da v dv -> dv -> da
backprop (AffineFunc _y0 dy) dv = unsafePerformIO $ do
    NodeMap.SomeSharedExprWithMap smap expr <- runRecoverSharing5 dy :: IO (NodeMap.SomeSharedExprWithMap a da v dv)
    let x' = Graph.convertGraph smap expr -- :: Graph.ForwardGraph s a da v dv
        dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv
    return (dv ⊗ dx')

backpropS :: (AdditiveGroup da, Num dv) => BVar b a da v dv -> da
backpropS x = backprop x 1
