{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
(
    Expr3(..), ExprArg(..), Term3(..), Expr2(..), zeroE, sumExpr2
)
where
--import Tensor(TensorProduct(..), LinearFunction, AFunction, transposeFunc)
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, AFunction2(AFunction2), identityFunc, AFunction1)
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv


data ExprArg p a da dv where
    ArgVar :: ExprArg p a da da
    ArgExpr :: p dv -> ExprArg p a da dv

data Term3 p a da dv where
    Func2 :: AFunction1 du dv -> ExprArg p a da du -> Term3 p a da dv

data Expr3 p a da dv = BasicVector dv => ExprSum [Term3 p a da dv]

--type Term2 a da = Term3 (Expr2 a da) a da

newtype Expr2 a da dv = Expr2 { unExpr2 :: Expr3 (Expr2 a da) a da dv }

zeroE :: BasicVector dv => Expr2 a da dv
zeroE = Expr2 (ExprSum [])

instance FullVector dv => AdditiveGroup (Expr2 a da dv) where
    zeroV = zeroE
    negateV x = Expr2 (ExprSum [Func2 negateFunc (ArgExpr x)])
    x ^+^ y = Expr2 (ExprSum [Func2 identityFunc (ArgExpr x), Func2 identityFunc (ArgExpr y)])
    x ^-^ y = Expr2 (ExprSum [Func2 identityFunc (ArgExpr x), Func2 negateFunc (ArgExpr y)])

instance FullVector dv => VectorSpace (Expr2 a da dv) where
    type Scalar (Expr2 a da dv) = Scalar dv
    a *^ v = Expr2 (ExprSum [Func2 (scaleFunc a) (ArgExpr v)])

instance FullVector dv => AdditiveGroup (ExprArg (Expr2 a da) a da dv) where
    zeroV = ArgExpr zeroV
    negateV x = ArgExpr (Expr2 (ExprSum [Func2 negateFunc x]))
    x ^+^ y = ArgExpr (Expr2 (ExprSum [Func2 identityFunc x, Func2 identityFunc y]))
    x ^-^ y = ArgExpr (Expr2 (ExprSum [Func2 identityFunc x, Func2 negateFunc y]))

instance FullVector dv => VectorSpace (ExprArg (Expr2 a da) a da dv) where
    type Scalar (ExprArg (Expr2 a da) a da dv) = Scalar (Expr2 a da dv)
    a *^ x = ArgExpr (Expr2 (ExprSum [Func2 (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr2 a da dv] -> Expr2 a da dv
sumExpr2 xs = Expr2 (ExprSum (wrap <$> xs))
    where wrap x = Func2 identityFunc (ArgExpr x)
{-
instance BasicVector v => TensorProduct (Term3 (Expr2 a da) a da v dv) a (VecBuilder v) where
    expr ⊗ a = case expr of
        Func2 f x -> case x of
            ArgVar -> f ⊗ a
            ArgExpr x' -> case x' of
                Expr2 (ExprSum _) -> f ⊗ (x' ⊗ a)

instance BasicVector v => TensorProduct (Expr2 a da v dv) a v where
    Expr2 (ExprSum xs) ⊗ a = _sumV [x ⊗ a | x <- xs]

instance BasicVector da => TensorProduct dv (Term3 (Expr2 a da) a da v dv) (VecBuilder da) where
    dv ⊗ expr = case expr of
        Func2 f x -> case x of
            ArgVar -> dv ⊗ f
            ArgExpr x' -> case x' of
                Expr2 (ExprSum _) -> (dv ⊗ f) ⊗ x'
instance BasicVector da => TensorProduct dv (Expr2 a da v dv) da where
    dv ⊗ Expr2 (ExprSum xs) = sumV [dv ⊗ x | x <- xs]
-}
-- >>> 1

-- Func :: LinearFunction b f u v du dv => f -> Expr' a da b u du -> Expr' a da b v dv
