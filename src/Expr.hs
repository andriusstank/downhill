{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
where
--import Tensor(TensorProduct(..), LinearFunction, AFunction, transposeFunc)
import Tensor
import Data.VectorSpace (AdditiveGroup, sumV)
import Data.Constraint
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv


data Expr a da v dv where
    Variable :: Expr a da a da
    Func :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv

data ExprArg a da v dv where
    ArgVar :: ExprArg a da a da
    ArgExpr :: Expr2 a da v dv -> ExprArg a da v dv

data Term2 a da v dv where
    Func2 :: AFunction u du v dv -> ExprArg a da u du -> Term2 a da v dv

data Expr2 a da v dv = (AdditiveGroup v, AdditiveGroup dv) => ExprSum [Term2 a da v dv]

-- Evaluate
instance TensorProduct (Expr a da v dv) a v where
    expr ⊗ a = case expr of
        Variable -> a
        Func f x -> f ⊗ (x ⊗ a)
        Sum xs -> sumV [x ⊗ a | x <- xs]

instance TensorProduct (Term2 a da v dv) a v where
    expr ⊗ a = case expr of
        Func2 f x -> case x of
            ArgVar -> f ⊗ a
            ArgExpr x' -> f ⊗ (x' ⊗ a)

instance TensorProduct (Expr2 a da v dv) a v where
    ExprSum xs ⊗ a = sumV [x ⊗ a | x <- xs]


-- Substitute
instance TensorProduct (Expr x dx v dv) (Expr a da x dx) (Expr a da v dv) where
    expr ⊗ x = case expr of
        Variable -> x
        Func f y -> Func f (y ⊗ x)
        Sum ys -> Sum [y ⊗ x | y <- ys]

-- Reverse mode evaluation
instance AdditiveGroup da => TensorProduct dv (Expr a da v dv) da where
    dv ⊗ expr = case expr of
        Variable -> dv
        Func f x -> (dv ⊗ f) ⊗ x
        Sum xs -> sumV [dv ⊗ x | x <- xs]

instance AdditiveGroup da => TensorProduct dv (Term2 a da v dv) da where
    dv ⊗ expr = case expr of
        Func2 f x -> case x of
            ArgVar -> dv ⊗ f
            ArgExpr x' -> (dv ⊗ f) ⊗ x'
instance AdditiveGroup da => TensorProduct dv (Expr2 a da v dv) da where
    dv ⊗ ExprSum xs = sumV [dv ⊗ x | x <- xs]

instance (AdditiveGroup u, AdditiveGroup du, AdditiveGroup v, AdditiveGroup dv) => LinearFunction (Expr u du v dv) u v du dv

instance LinearFunction' Expr where
    transpose = \case
        Variable -> Variable
        Func f x -> x' ⊗ f'
            where x' = transpose x
                  f' = Func (transpose f) Variable
        Sum xs -> Sum (transpose <$> xs)
    transposeC = Sub Dict
-- >>> 1

-- Func :: LinearFunction b f u v du dv => f -> Expr' a da b u du -> Expr' a da b v dv

