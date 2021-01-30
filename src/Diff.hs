{-# LANGUAGE TupleSections #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language GADTs #-}

module Diff
(
    BVar, BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip
)
where

import Expr(ExprArg(ArgExpr), Term3(Func2), zeroE)
import Prelude (Monad(return), Num, IO, ($))
import Expr(ExprArg(ArgVar), Expr2(Expr2), Expr3(ExprSum))
import Affine (AffineFunc(AffineFunc))
import Tensor (TensorProduct((⊗)))
import NodeMap (runRecoverSharing5)

import qualified Prelude
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (AFunction2, transposeFunc2, ProdVector, FullVectors, fstF, sndF, BasicVector,  BasicVectors)

type BVar b a da v dv = AffineFunc b (ExprArg (Expr2 a da) a da v dv)

type BVarS a = BVar a a a a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: BasicVectors v dv => b -> BVar b a da v dv
constant x = AffineFunc x (ArgExpr zeroE)

var :: b -> BVar b v dv v dv
var x = AffineFunc x ArgVar

backprop' :: forall a da v dv. BasicVector da => Expr2 a da v dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    NodeMap.SomeSharedExprWithMap smap expr <- runRecoverSharing5 dy :: IO (NodeMap.SomeSharedExprWithMap a da v dv)
    let x' = Graph.convertGraph smap expr -- :: Graph.ForwardGraph s a da v dv
        dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv
    return (dv ⊗ dx')

backprop :: forall b a da v dv. BasicVector da => BVar b a da v dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    ArgVar -> dv
    ArgExpr x -> backprop' x dv

backpropS :: (BasicVector da, Num dv) => BVar b a da v dv -> da
backpropS x = backprop x 1

fstT :: FullVectors u du => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> Term3 (Expr2 a da) a da u du
fstT x = Func2 fstF x
fstA :: FullVectors u du => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> ExprArg (Expr2 a da) a da u du
fstA x = ArgExpr (Expr2 (ExprSum [fstT x]))
fst :: FullVectors u du => BVar (b1, b2) a da (u, v) (du, dv) -> BVar b1 a da u du
fst (AffineFunc y0 dy) = AffineFunc (Prelude.fst y0) (fstA dy)

sndT :: FullVectors v dv => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> Term3 (Expr2 a da) a da v dv
sndT x = Func2 sndF x
sndA :: FullVectors v dv => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> ExprArg (Expr2 a da) a da v dv
sndA x = ArgExpr (Expr2 (ExprSum [sndT x]))
snd :: FullVectors v dv => BVar (b1, b2) a da (u, v) (du, dv) -> BVar b2 a da v dv
snd (AffineFunc y0 dy) = AffineFunc (Prelude.snd y0) (sndA dy)

zipA :: forall a da u du v dv. (ProdVector u, ProdVector du, ProdVector v, ProdVector dv) => ExprArg (Expr2 a da) a da u du -> ExprArg (Expr2 a da) a da v dv -> ExprArg (Expr2 a da) a da (u, v) (du, dv)
zipA x y = ArgExpr (Expr2 (ExprSum [Func2 intoFst x, Func2 intoSnd y]))
    where intoFst :: (ProdVector u, ProdVector du) => AFunction2 u du (u, v) (du, dv)
          intoFst = transposeFunc2 fstF
          intoSnd :: (ProdVector u, ProdVector du) => AFunction2 v dv (u, v) (du, dv)
          intoSnd = transposeFunc2 sndF

zip :: (ProdVector u, ProdVector du, ProdVector v, ProdVector dv) => BVar b a da u du -> BVar c a da v dv -> BVar (b, c) a da (u, v) (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
