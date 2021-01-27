{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
(
    Expr3(..), ExprArg(..), Term3(..), Expr2(..), sumExpr2
)
where
--import Tensor(TensorProduct(..), LinearFunction, AFunction, transposeFunc)
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv


data ExprArg p a da v dv where
    ArgVar :: ExprArg p a da a da
    ArgExpr :: p v dv -> ExprArg p a da v dv

data Term3 p a da v dv where
    Func2 :: AFunction u du v dv -> ExprArg p a da u du -> Term3 p a da v dv

data Expr3 p a da v dv = (AdditiveGroup v, AdditiveGroup dv) => ExprSum [Term3 p a da v dv]

--type Term2 a da = Term3 (Expr2 a da) a da

newtype Expr2 a da v dv = Expr2 { unExpr2 :: Expr3 (Expr2 a da) a da v dv }

instance (AdditiveGroup v, AdditiveGroup dv) => AdditiveGroup (Expr2 a da v dv) where
    zeroV = Expr2 (ExprSum [])
    negateV x = Expr2 (ExprSum [Func2 NegateFunc (ArgExpr x)])
    x ^+^ y = Expr2 (ExprSum [Func2 IndentityFunc (ArgExpr x), Func2 IndentityFunc (ArgExpr y)])
    x ^-^ y = Expr2 (ExprSum [Func2 IndentityFunc (ArgExpr x), Func2 NegateFunc (ArgExpr y)])

instance (VectorSpace v, VectorSpace dv, Scalar v ~ Scalar dv) => VectorSpace (Expr2 a da v dv) where
    type Scalar (Expr2 a da v dv) = Scalar v
    a *^ v = Expr2 (ExprSum [Func2 (ScaleFunc a) (ArgExpr v)])

instance (AdditiveGroup v, AdditiveGroup dv) => AdditiveGroup (ExprArg (Expr2 a da) a da v dv) where
    zeroV = ArgExpr zeroV
    negateV x = ArgExpr (Expr2 (ExprSum [Func2 NegateFunc x]))
    x ^+^ y = ArgExpr (Expr2 (ExprSum [Func2 IndentityFunc x, Func2 IndentityFunc y]))
    x ^-^ y = ArgExpr (Expr2 (ExprSum [Func2 IndentityFunc x, Func2 NegateFunc y]))

instance (VectorSpace v, VectorSpace dv, Scalar v ~ Scalar dv) => VectorSpace (ExprArg (Expr2 a da) a da v dv) where
    type Scalar (ExprArg (Expr2 a da) a da v dv) = Scalar (Expr2 a da v dv)
    a *^ x = ArgExpr (Expr2 (ExprSum [Func2 (ScaleFunc a) x]))

sumExpr2 :: (AdditiveGroup v, AdditiveGroup dv) => [Expr2 a da v dv] -> Expr2 a da v dv
sumExpr2 xs = Expr2 (ExprSum (wrap <$> xs))
    where wrap x = Func2 IndentityFunc (ArgExpr x)

instance TensorProduct (Term3 (Expr2 a da) a da v dv) a v where
    expr ⊗ a = case expr of
        Func2 f x -> case x of
            ArgVar -> f ⊗ a
            ArgExpr x' -> f ⊗ (x' ⊗ a)

instance TensorProduct (Expr2 a da v dv) a v where
    Expr2 (ExprSum xs) ⊗ a = sumV [x ⊗ a | x <- xs]



instance AdditiveGroup da => TensorProduct dv (Term3 (Expr2 a da) a da v dv) da where
    dv ⊗ expr = case expr of
        Func2 f x -> case x of
            ArgVar -> dv ⊗ f
            ArgExpr x' -> (dv ⊗ f) ⊗ x'
instance AdditiveGroup da => TensorProduct dv (Expr2 a da v dv) da where
    dv ⊗ Expr2 (ExprSum xs) = sumV [dv ⊗ x | x <- xs]

-- >>> 1

-- Func :: LinearFunction b f u v du dv => f -> Expr' a da b u du -> Expr' a da b v dv
