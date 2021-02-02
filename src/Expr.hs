{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
(
    Expr3(..), ExprArg(..), Term3(..), Expr2(..), zeroE, sumExpr2, expr2to5, expr5to2
)
where
--import Tensor(TensorProduct(..), LinearFunction, AFunction, transposeFunc)
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, AFunction2(AFunction2), identityFunc, AFunction1)
import EType (Expr4 (Expr4Sum))
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv


data ExprArg p da dv where
    ArgVar :: ExprArg p da da
    ArgExpr :: p dv -> ExprArg p da dv

data Term3 p da dv where
    Func2 :: AFunction1 du dv -> ExprArg p da du -> Term3 p da dv

data Expr3 p da dv = BasicVector dv => ExprSum [Term3 p da dv]

newtype Expr2 da dv = Expr2 { unExpr2 :: Expr3 (Expr2 da) da dv }

newtype Expr5 da dv = Expr5 { unExpr5 :: Expr4 (Term3 (Expr5 da) da) dv }

expr2to5 :: Expr2 da dv -> Expr5 da dv
expr2to5 (Expr2 (ExprSum xs)) = Expr5 (Expr4Sum (goTerm <$> xs))
    where goTerm :: Term3 (Expr2 da) da dv -> Term3 (Expr5 da) da dv
          goTerm = \case
            Func2 f arg -> Func2 f (goArg arg)
          goArg :: ExprArg (Expr2 da) da du -> ExprArg (Expr5 da) da du
          goArg = \case
            ArgVar -> ArgVar
            ArgExpr x -> ArgExpr (expr2to5 x)

expr5to2 :: Expr5 da dv -> Expr2 da dv
expr5to2 (Expr5 (Expr4Sum xs)) = Expr2 (ExprSum (goTerm <$> xs))
    where goTerm :: Term3 (Expr5 da) da dv -> Term3 (Expr2 da) da dv
          goTerm = \case
            Func2 f arg -> Func2 f (goArg arg)
          goArg :: ExprArg (Expr5 da) da du -> ExprArg (Expr2 da) da du
          goArg = \case
            ArgVar -> ArgVar
            ArgExpr x -> ArgExpr (expr5to2 x)


zeroE :: BasicVector dv => Expr2 da dv
zeroE = Expr2 (ExprSum [])

instance FullVector dv => AdditiveGroup (Expr2 da dv) where
    zeroV = zeroE
    negateV x = Expr2 (ExprSum [Func2 negateFunc (ArgExpr x)])
    x ^+^ y = Expr2 (ExprSum [Func2 identityFunc (ArgExpr x), Func2 identityFunc (ArgExpr y)])
    x ^-^ y = Expr2 (ExprSum [Func2 identityFunc (ArgExpr x), Func2 negateFunc (ArgExpr y)])

instance FullVector dv => VectorSpace (Expr2 da dv) where
    type Scalar (Expr2 da dv) = Scalar dv
    a *^ v = Expr2 (ExprSum [Func2 (scaleFunc a) (ArgExpr v)])

instance FullVector dv => AdditiveGroup (ExprArg (Expr2 da) da dv) where
    zeroV = ArgExpr zeroV
    negateV x = ArgExpr (Expr2 (ExprSum [Func2 negateFunc x]))
    x ^+^ y = ArgExpr (Expr2 (ExprSum [Func2 identityFunc x, Func2 identityFunc y]))
    x ^-^ y = ArgExpr (Expr2 (ExprSum [Func2 identityFunc x, Func2 negateFunc y]))

instance FullVector dv => VectorSpace (ExprArg (Expr2 da) da dv) where
    type Scalar (ExprArg (Expr2 da) da dv) = Scalar (Expr2 da dv)
    a *^ x = ArgExpr (Expr2 (ExprSum [Func2 (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr2 da dv] -> Expr2 da dv
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

