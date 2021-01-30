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
import Notensor (FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, AFunction2(AFunction2), identityFunc)
-- class (TensorProduct f u v, TensorProduct dv f du) => LinearFunction b f u v du dv | f -> b where

--data SomeLinearFunc b v dv where
--    SomeLinearFunc :: LinearFunction b f u v du dv => SomeLinearFunc b v dv


data ExprArg p a da v dv where
    ArgVar :: ExprArg p a da a da
    ArgExpr :: p v dv -> ExprArg p a da v dv

data Term3 p a da v dv where
    Func2 :: AFunction2 u du v dv -> ExprArg p a da u du -> Term3 p a da v dv

data Expr3 p a da v dv = BasicVectors v dv => ExprSum [Term3 p a da v dv]

--type Term2 a da = Term3 (Expr2 a da) a da

newtype Expr2 a da v dv = Expr2 { unExpr2 :: Expr3 (Expr2 a da) a da v dv }

--zeroE :: Expr2 a da v dv
--zeroE = Expr2 (ExprSum [])

instance FullVectors v dv => AdditiveGroup (Expr2 a da v dv) where
    zeroV = Expr2 (ExprSum [])
    negateV x = Expr2 (ExprSum [Func2 negateFunc (ArgExpr x)])
    x ^+^ y = Expr2 (ExprSum [Func2 identityFunc (ArgExpr x), Func2 identityFunc (ArgExpr y)])
    x ^-^ y = Expr2 (ExprSum [Func2 identityFunc (ArgExpr x), Func2 negateFunc (ArgExpr y)])

instance FullVectors v dv => VectorSpace (Expr2 a da v dv) where
    type Scalar (Expr2 a da v dv) = Scalar v
    a *^ v = Expr2 (ExprSum [Func2 (scaleFunc a) (ArgExpr v)])

instance FullVectors v dv => AdditiveGroup (ExprArg (Expr2 a da) a da v dv) where
    zeroV = ArgExpr zeroV
    negateV x = ArgExpr (Expr2 (ExprSum [Func2 negateFunc x]))
    x ^+^ y = ArgExpr (Expr2 (ExprSum [Func2 identityFunc x, Func2 identityFunc y]))
    x ^-^ y = ArgExpr (Expr2 (ExprSum [Func2 identityFunc x, Func2 negateFunc y]))

instance FullVectors v dv => VectorSpace (ExprArg (Expr2 a da) a da v dv) where
    type Scalar (ExprArg (Expr2 a da) a da v dv) = Scalar (Expr2 a da v dv)
    a *^ x = ArgExpr (Expr2 (ExprSum [Func2 (scaleFunc a) x]))

sumExpr2 :: FullVectors v dv => [Expr2 a da v dv] -> Expr2 a da v dv
sumExpr2 xs = Expr2 (ExprSum (wrap <$> xs))
    where wrap x = Func2 identityFunc (ArgExpr x)

instance BasicVector v => TensorProduct (Term3 (Expr2 a da) a da v dv) a v where
    expr ⊗ a = case expr of
        Func2 f x -> case x of
            ArgVar -> f ⊗ a
            ArgExpr x' -> case x' of
                Expr2 (ExprSum _) -> f ⊗ (x' ⊗ a)

instance FullVector v => TensorProduct (Expr2 a da v dv) a v where
    Expr2 (ExprSum xs) ⊗ a = sumV [x ⊗ a | x <- xs]

instance FullVector da => TensorProduct dv (Term3 (Expr2 a da) a da v dv) da where
    dv ⊗ expr = case expr of
        Func2 f x -> case x of
            ArgVar -> dv ⊗ f
            ArgExpr x' -> case x' of
                Expr2 (ExprSum _) -> (dv ⊗ f) ⊗ x'
instance FullVector da => TensorProduct dv (Expr2 a da v dv) da where
    dv ⊗ Expr2 (ExprSum xs) = sumV [dv ⊗ x | x <- xs]

-- >>> 1

-- Func :: LinearFunction b f u v du dv => f -> Expr' a da b u du -> Expr' a da b v dv
