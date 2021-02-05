{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
(
    ExprArg(..), Term3(..), Expr5(..), zeroE, sumExpr2
)
where
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, AFunction2(AFunction2), identityFunc, AFunction1)
import EType (VectorSum (VectorSum))


data ExprArg p da dv where
    ArgVar :: ExprArg p da da
    ArgExpr :: p dv -> ExprArg p da dv

data Term3 p da dv where
    Func2 :: AFunction1 du dv -> ExprArg p da du -> Term3 p da dv

newtype Expr5 da dv = Expr5 { unExpr5 :: VectorSum (Term3 (Expr5 da) da) dv }

zeroE :: BasicVector dv => Expr5 da dv
zeroE = Expr5 (VectorSum [])

instance FullVector dv => AdditiveGroup (Expr5 da dv) where
    zeroV = zeroE
    negateV x = Expr5 (VectorSum [Func2 negateFunc (ArgExpr x)])
    x ^+^ y = Expr5 (VectorSum [Func2 identityFunc (ArgExpr x), Func2 identityFunc (ArgExpr y)])
    x ^-^ y = Expr5 (VectorSum [Func2 identityFunc (ArgExpr x), Func2 negateFunc (ArgExpr y)])

instance FullVector dv => VectorSpace (Expr5 da dv) where
    type Scalar (Expr5 da dv) = Scalar dv
    a *^ v = Expr5 (VectorSum [Func2 (scaleFunc a) (ArgExpr v)])

instance FullVector dv => AdditiveGroup (ExprArg (Expr5 da) da dv) where
    zeroV = ArgExpr zeroV
    negateV x = ArgExpr (Expr5 (VectorSum [Func2 negateFunc x]))
    x ^+^ y = ArgExpr (Expr5 (VectorSum [Func2 identityFunc x, Func2 identityFunc y]))
    x ^-^ y = ArgExpr (Expr5 (VectorSum [Func2 identityFunc x, Func2 negateFunc y]))

instance FullVector dv => VectorSpace (ExprArg (Expr5 da) da dv) where
    type Scalar (ExprArg (Expr5 da) da dv) = Scalar (Expr5 da dv)
    a *^ x = ArgExpr (Expr5 (VectorSum [Func2 (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr5 da dv] -> Expr5 da dv
sumExpr2 xs = Expr5 (VectorSum (wrap <$> xs))
    where wrap x = Func2 identityFunc (ArgExpr x)
