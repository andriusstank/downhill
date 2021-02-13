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

import Expr(ExprArg(ArgExpr), Term3(Func2), zeroE, Expr5(Expr5))
import Prelude (Monad(return), Num, IO, ($))
import Expr(ExprArg(ArgVar))
import Affine (AffineFunc(AffineFunc))
import Tensor (TensorProduct((⊗)))
import NodeMap (runRecoverSharing5)

import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (ProdVector, BasicVector, fstF1, sndF1, intoFst, intoSnd, AFunction1)
import EType (VectorSum(VectorSum))

type BVar b da dv = AffineFunc b (ExprArg (Expr5 da) da dv)

type BVarS a = BVar a a a

bvarValue :: AffineFunc b dv -> b
bvarValue (AffineFunc y0 _dy) = y0

constant :: BasicVector dv => b -> BVar b da dv
constant x = AffineFunc x (ArgExpr zeroE)

var :: b -> BVar b dv dv
var x = AffineFunc x ArgVar

backprop' :: forall da dv. BasicVector da => Expr5 da dv -> dv -> da
backprop' dy dv = unsafePerformIO $ do
    NodeMap.SomeSharedExprWithMap smap expr <- runRecoverSharing5 dy :: IO (NodeMap.SomeSharedExprWithMap da dv)
    let x' = Graph.convertGraph smap expr -- :: Graph.ForwardGraph s a da v dv
        dx' = Graph.flipGraph x' -- :: Graph.BackwardGraph s' a da v dv
    return (dv ⊗ dx')

backprop :: forall b da dv. BasicVector da => BVar b da dv -> dv -> da
backprop (AffineFunc _y0 y) dv = case y of
    ArgVar -> dv
    ArgExpr x -> backprop' x dv

backpropS :: (BasicVector da, Num dv) => BVar b da dv -> da
backpropS x = backprop x 1

liftFunc1 
  :: forall u v da dv du. 
     BasicVector dv
  => (u -> (v, AFunction1 du dv))
  -> AffineFunc u (ExprArg (Expr5 da) da du)
  -> AffineFunc v (ExprArg (Expr5 da) da dv)
liftFunc1 f (AffineFunc x0 dx) = AffineFunc y0 expr
    where term :: Term3 (Expr5 da) AFunction1 da dv
          term = Func2 df dx
          expr :: ExprArg (Expr5 da) da dv
          expr = ArgExpr (Expr5 (VectorSum [term]))
          (y0, df) = f x0


fst :: forall b1 b2 da du dv. ProdVector du => BVar (b1, b2) da (du, dv) -> BVar b1 da du
fst = liftFunc1 go
    where go (x, _) = (x, fstF1)

snd :: forall b1 b2 da du dv. ProdVector dv => BVar (b1, b2) da (du, dv) -> BVar b2 da dv
snd = liftFunc1 go
    where go (_, x) = (x, sndF1)

zipA
  :: forall da du dv. (ProdVector du, ProdVector dv)
  => ExprArg (Expr5 da) da du
  -> ExprArg (Expr5 da) da dv
  -> ExprArg (Expr5 da) da (du, dv)
zipA x y = ArgExpr (Expr5 (VectorSum [Func2 intoFst x, Func2 intoSnd y]))

zip :: (ProdVector du, ProdVector dv) => BVar b da du -> BVar c da dv -> BVar (b, c) da (du, dv)
zip (AffineFunc x dx) (AffineFunc y dy) = AffineFunc (x, y) (zipA dx dy)
