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

import Expr(ExprArg(ArgExpr), Term3(Func2))
import Prelude (Monad(return), Num, IO, ($))
import Expr(ExprArg(ArgVar), Expr2(Expr2), Expr3(ExprSum), Term3(Func2))
import Affine (AffineFunc(AffineFunc))
import Data.VectorSpace (AdditiveGroup(zeroV))
import Tensor (TensorProduct((⊗)), AFunction(BlackBoxFunc, IndentityFunc))
import NodeMap (runRecoverSharing5)

import qualified Prelude
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)

type BVar b a da v dv = AffineFunc b (ExprArg (Expr2 a da) a da v dv)

type BVarS a = BVar a a a a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: (AdditiveGroup v, AdditiveGroup dv) => b -> BVar b a da v dv
constant x = AffineFunc x (ArgExpr zeroV)

var :: (AdditiveGroup v, AdditiveGroup dv) => b -> BVar b v dv v dv
var x = AffineFunc x ArgVar

backprop' :: forall a da v dv. AdditiveGroup da => Expr2 a da v dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    NodeMap.SomeSharedExprWithMap smap expr <- runRecoverSharing5 dy :: IO (NodeMap.SomeSharedExprWithMap a da v dv)
    let x' = Graph.convertGraph smap expr -- :: Graph.ForwardGraph s a da v dv
        dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv
    return (dv ⊗ dx')

backprop :: forall b a da v dv. AdditiveGroup da => BVar b a da v dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    ArgVar -> dv
    ArgExpr x -> backprop' x dv

backpropS :: (AdditiveGroup da, Num dv) => BVar b a da v dv -> da
backpropS x = backprop x 1

fstT :: AdditiveGroup dv => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> Term3 (Expr2 a da) a da u du
fstT x = Func2 (BlackBoxFunc Prelude.fst (, zeroV)) x
fstA :: (AdditiveGroup u, AdditiveGroup du, AdditiveGroup  dv) => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> ExprArg (Expr2 a da) a da u du
fstA x = ArgExpr (Expr2 (ExprSum [fstT x]))
fst :: (AdditiveGroup u, AdditiveGroup du, AdditiveGroup dv) => BVar (b1, b2) a da (u, v) (du, dv) -> BVar b1 a da u du
fst (AffineFunc y0 dy) = AffineFunc (Prelude.fst y0) (fstA dy)

sndT :: AdditiveGroup du => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> Term3 (Expr2 a da) a da v dv
sndT x = Func2 (BlackBoxFunc Prelude.snd (zeroV, )) x
sndA :: (AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => ExprArg (Expr2 a da) a da (u, v) (du, dv) -> ExprArg (Expr2 a da) a da v dv
sndA x = ArgExpr (Expr2 (ExprSum [sndT x]))
snd :: (AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => BVar (b1, b2) a da (u, v) (du, dv) -> BVar b2 a da v dv
snd (AffineFunc y0 dy) = AffineFunc (Prelude.snd y0) (sndA dy)

zipA :: forall a da u du v dv. (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => ExprArg (Expr2 a da) a da u du -> ExprArg (Expr2 a da) a da v dv -> ExprArg (Expr2 a da) a da (u, v) (du, dv)
zipA x y = ArgExpr (Expr2 (ExprSum [intoFst x, intoSnd y]))
    where intoFst :: ExprArg (Expr2 a da) a da u du -> Term3 (Expr2 a da) a da (u, v) (du, dv)
          intoFst z = Func2 (BlackBoxFunc (, zeroV) Prelude.fst) z
          intoSnd :: ExprArg (Expr2 a da) a da v dv -> Term3 (Expr2 a da) a da (u, v) (du, dv)
          intoSnd z = Func2 (BlackBoxFunc (zeroV,) Prelude.snd) z

zip :: (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => BVar b a da u du -> BVar c a da v dv -> BVar (b, c) a da (u, v) (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
