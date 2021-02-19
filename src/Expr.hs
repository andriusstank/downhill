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
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, identityFunc, BackFunc)
import EType (Node(Node), Endpoint (InnerNode), Edge(Edge))

newtype Expr5 e da dv = Expr5 { unExpr5 :: Node (Expr5 e da) e da dv }

zeroE :: BasicVector dv => Expr5 e da dv
zeroE = Expr5 (Node [])

instance FullVector dv => AdditiveGroup (Expr5 BackFunc da dv) where
    zeroV = zeroE
    negateV x = Expr5 (Node [Edge negateFunc (InnerNode x)])
    x ^+^ y = Expr5 (Node [Edge identityFunc (InnerNode x), Edge identityFunc (InnerNode y)])
    x ^-^ y = Expr5 (Node [Edge identityFunc (InnerNode x), Edge negateFunc (InnerNode y)])

instance FullVector dv => VectorSpace (Expr5 BackFunc da dv) where
    type Scalar (Expr5 BackFunc da dv) = Scalar dv
    a *^ v = Expr5 (Node [Edge (scaleFunc a) (InnerNode v)])

instance FullVector dv => AdditiveGroup (Endpoint (Expr5 BackFunc da) da dv) where
    zeroV = InnerNode zeroV
    negateV x = InnerNode (Expr5 (Node [Edge negateFunc x]))
    x ^+^ y = InnerNode (Expr5 (Node [Edge identityFunc x, Edge identityFunc y]))
    x ^-^ y = InnerNode (Expr5 (Node [Edge identityFunc x, Edge negateFunc y]))

instance FullVector dv => VectorSpace (Endpoint (Expr5 BackFunc da) da dv) where
    type Scalar (Endpoint (Expr5 BackFunc da) da dv) = Scalar (Expr5 BackFunc da dv)
    a *^ x = InnerNode (Expr5 (Node [Edge (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr5 BackFunc da dv] -> Expr5 BackFunc da dv
sumExpr2 xs = Expr5 (Node (wrap <$> xs))
    where wrap x = Edge identityFunc (InnerNode x)
