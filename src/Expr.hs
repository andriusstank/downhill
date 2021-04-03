{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE TypeApplications #-}
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

data Endpoint' p da dv where
    SourceNode' :: Endpoint' p da da
    InnerNode' :: p dv -> Endpoint' p da dv

data Edge' p f da dv where
    Edge' :: f du dv -> Endpoint' p da du -> Edge' p f da dv

data Node' p f da dv = BasicVector dv => Node' [Edge' p f da dv]

data Expr5 e da dv where
    Expr5 :: Node' (Expr5 e da) e da dv -> Expr5 e da dv
    Expr5Subs :: LinearFunc5 e dx dv -> Expr5 e da dx -> Expr5 e da dv

newtype LinearFunc5 e da dv = LinearFunc5 (Endpoint' (Expr5 e da) da dv)

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
        where go :: Endpoint' (Expr5 e b) b c -> Endpoint' (Expr5 e a) a b -> Endpoint' (Expr5 e a) a c
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

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Endpoint' (Expr5 e da) da dv) where
    zeroV = InnerNode' zeroV
    negateV x = InnerNode' (Expr5 (Node' [Edge' negateFunc x]))
    x ^+^ y = InnerNode' (Expr5 (Node' [Edge' identityFunc x, Edge' identityFunc y]))
    x ^-^ y = InnerNode' (Expr5 (Node' [Edge' identityFunc x, Edge' negateFunc y]))

instance FullVector dv => VectorSpace (Endpoint' (Expr5 BackFunc da) da dv) where
    type Scalar (Endpoint' (Expr5 BackFunc da) da dv) = Scalar (Expr5 BackFunc da dv)
    a *^ x = InnerNode' (Expr5 (Node' [Edge' (scaleFunc a) x]))

sumExpr2 :: FullVector dv => [Expr5 BackFunc da dv] -> Expr5 BackFunc da dv
sumExpr2 xs = Expr5 (Node' (wrap <$> xs))
    where wrap x = Edge' identityFunc (InnerNode' x)
