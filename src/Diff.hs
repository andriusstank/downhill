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








import Expr(ExprArg(ArgExpr), Term3(Func2), zeroE, Expr5(Expr5), Expr2)
import Prelude (Monad(return), Num, IO, ($))
import Expr(ExprArg(ArgVar), Expr3(ExprSum))
import Affine (AffineFunc(AffineFunc))
import Tensor (TensorProduct((⊗)))
import NodeMap (runRecoverSharing5)

import qualified Prelude
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (ProdVector, BasicVector, FullVector, fstF1, sndF1, intoFst, intoSnd)
import EType (Expr4(Expr4Sum))

type BVar b a da v dv = AffineFunc b (ExprArg (Expr5 da) da dv)

type BVarS a = BVar a a a a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: BasicVector dv => b -> BVar b a da v dv
constant x = AffineFunc x (ArgExpr zeroE)

var :: b -> BVar b v dv v dv
var x = AffineFunc x ArgVar

backprop' :: forall da dv. BasicVector da => Expr5 da dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    NodeMap.SomeSharedExprWithMap smap expr <- runRecoverSharing5 dy :: IO (NodeMap.SomeSharedExprWithMap da dv)
    let x' = Graph.convertGraph smap expr -- :: Graph.ForwardGraph s a da v dv
        dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv
    return (dv ⊗ dx')

backprop :: forall b a da v dv. BasicVector da => BVar b a da v dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    ArgVar -> dv
    ArgExpr x -> backprop' x dv

backpropS :: (BasicVector da, Num dv) => BVar b a da v dv -> da
backpropS x = backprop x 1

fstT :: FullVector du => ExprArg (Expr2 da) da (du, dv) -> Term3 (Expr2 da) da du
fstT x = Func2 fstF1 x
fstA :: FullVector du => ExprArg (Expr2 da) da (du, dv) -> ExprArg (Expr2 da) da du
fstA x = ArgExpr (Expr5 (Expr4Sum [fstT x]))
fst :: FullVector du => BVar (b1, b2) a da (u, v) (du, dv) -> BVar b1 a da u du
fst (AffineFunc y0 dy) = AffineFunc (Prelude.fst y0) (fstA dy)

sndT :: FullVector dv => ExprArg (Expr2 da) da (du, dv) -> Term3 (Expr2 da) da dv
sndT x = Func2 sndF1 x
sndA :: FullVector dv => ExprArg (Expr2 da) da (du, dv) -> ExprArg (Expr2 da) da dv
sndA x = ArgExpr (Expr5 (Expr4Sum [sndT x]))
snd :: FullVector dv => BVar (b1, b2) a da (u, v) (du, dv) -> BVar b2 a da v dv
snd (AffineFunc y0 dy) = AffineFunc (Prelude.snd y0) (sndA dy)

zipA :: forall da du dv. (ProdVector du, ProdVector dv) => ExprArg (Expr2 da) da du -> ExprArg (Expr2 da) da dv -> ExprArg (Expr2 da) da (du, dv)
zipA x y = ArgExpr (Expr5 (Expr4Sum [Func2 intoFst x, Func2 intoSnd y]))

zip :: (ProdVector du, ProdVector dv) => BVar b a da u du -> BVar c a da v dv -> BVar (b, c) a da (u, v) (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
