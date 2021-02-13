{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Expr
(
    Expr5(..), zeroE, sumExpr2
)
where
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, AFunction2(AFunction2), identityFunc, AFunction1)
import EType (VectorSum (VectorSum), Endpoint (InnerNode), Edge(Edge))

newtype Expr5 da dv = Expr5 { unExpr5 :: VectorSum (Edge (Expr5 da) AFunction1 da) dv }

zeroE :: BasicVector dv => Expr5 da dv
zeroE = Expr5 (VectorSum [])

instance FullVector dv => AdditiveGroup (Expr5 da dv) where
    zeroV = zeroE
    negateV x = Expr5 (VectorSum [Edge negateFunc (InnerNode x)])
    x ^+^ y = Expr5 (VectorSum [Edge identityFunc (InnerNode x), Edge identityFunc (InnerNode y)])
    x ^-^ y = Expr5 (VectorSum [Edge identityFunc (InnerNode x), Edge negateFunc (InnerNode y)])

instance FullVector dv => VectorSpace (Expr5 da dv) where
    type Scalar (Expr5 da dv) = Scalar dv
    a *^ v = Expr5 (VectorSum [Edge (scaleFunc a) (InnerNode v)])

instance FullVector dv => AdditiveGroup (Endpoint (Expr5 da) da dv) where
    zeroV = InnerNode zeroV
    negateV x = InnerNode (Expr5 (VectorSum [Edge negateFunc x]))
    x ^+^ y = InnerNode (Expr5 (VectorSum [Edge identityFunc x, Edge identityFunc y]))
    x ^-^ y = InnerNode (Expr5 (VectorSum [Edge identityFunc x, Edge negateFunc y]))

instance FullVector dv => VectorSpace (Endpoint (Expr5 da) da dv) where
    type Scalar (Endpoint (Expr5 da) da dv) = Scalar (Expr5 da dv)
    a *^ x = InnerNode (Expr5 (VectorSum [Edge (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr5 da dv] -> Expr5 da dv
sumExpr2 xs = Expr5 (VectorSum (wrap <$> xs))
    where wrap x = Edge identityFunc (InnerNode x)
