{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Expr
(
    Expr5(..), zeroE, sumExpr2, LinearFunc5(..),
    Endpoint'(..), Edge'(..), Node'(..)
)
where
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, identityFunc, BackFunc, LinearEdge)
import EType (Node(Node), Endpoint (InnerNode, SourceNode), Edge(Edge))
import Control.Category (Category(..))

data Endpoint' e da dv where
    SourceNode' :: Endpoint' e da da
    InnerNode' :: Expr5 e da dv -> Endpoint' e da dv

data Edge' e f da dv where
    Edge' :: f du dv -> Endpoint' e da du -> Edge' e f da dv

data Node' e f da dv = BasicVector dv => Node' [Edge' e f da dv]

data Expr5 e da dv where
    Expr5 :: Node' e e da dv -> Expr5 e da dv
    Expr5Subs :: LinearFunc5 e dx dv -> Expr5 e da dx -> Expr5 e da dv

newtype LinearFunc5 e da dv = LinearFunc5 (Endpoint' e da dv)

instance (LinearEdge e, FullVector dv) => AdditiveGroup (LinearFunc5 e da dv) where
    zeroV = LinearFunc5 (InnerNode' zeroE)
    negateV (LinearFunc5 x) = LinearFunc5 (negateV x)
    LinearFunc5 x ^+^ LinearFunc5 y = LinearFunc5 (InnerNode' (Expr5 (Node' [Edge' identityFunc x, Edge' identityFunc y])))
    LinearFunc5 x ^-^ LinearFunc5 y = LinearFunc5 (InnerNode' (Expr5 (Node' [Edge' identityFunc x, Edge' negateFunc y])))

instance (LinearEdge e, FullVector dv) => VectorSpace (LinearFunc5 e da dv) where
    type Scalar (LinearFunc5 e da dv) = Scalar dv
    a *^ LinearFunc5 v = LinearFunc5 (InnerNode' (Expr5 (Node' [Edge' (scaleFunc a) v])))

instance Category (LinearFunc5 e) where
    id = LinearFunc5 SourceNode'
    LinearFunc5 x . LinearFunc5 y = LinearFunc5 (go x y)
        where go :: Endpoint' e b c -> Endpoint' e a b -> Endpoint' e a c
              go SourceNode' y' = y'
              go x' SourceNode'  = x'
              go (InnerNode' x') (InnerNode' y') = InnerNode' (Expr5Subs (LinearFunc5 (InnerNode' x')) y')

zeroE :: BasicVector dv => Expr5 e da dv
zeroE = Expr5 (Node' [])

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Expr5 e da dv) where
    zeroV = zeroE
    negateV x = Expr5 (Node' [Edge' negateFunc (InnerNode' x)])
    x ^+^ y = Expr5 (Node' [Edge' identityFunc (InnerNode' x), Edge' identityFunc (InnerNode' y)])
    x ^-^ y = Expr5 (Node' [Edge' identityFunc (InnerNode' x), Edge' negateFunc (InnerNode' y)])

instance FullVector dv => VectorSpace (Expr5 BackFunc da dv) where
    type Scalar (Expr5 BackFunc da dv) = Scalar dv
    a *^ v = Expr5 (Node' [Edge' (scaleFunc a) (InnerNode' v)])

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Endpoint' e da dv) where
    zeroV = InnerNode' zeroV
    negateV x = InnerNode' (Expr5 (Node' [Edge' negateFunc x]))
    x ^+^ y = InnerNode' (Expr5 (Node' [Edge' identityFunc x, Edge' identityFunc y]))
    x ^-^ y = InnerNode' (Expr5 (Node' [Edge' identityFunc x, Edge' negateFunc y]))

instance FullVector dv => VectorSpace (Endpoint' BackFunc da dv) where
    type Scalar (Endpoint' BackFunc da dv) = Scalar (Expr5 BackFunc da dv)
    a *^ x = InnerNode' (Expr5 (Node' [Edge' (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr5 BackFunc da dv] -> Expr5 BackFunc da dv
sumExpr2 xs = Expr5 (Node' (wrap <$> xs))
    where wrap x = Edge' identityFunc (InnerNode' x)
