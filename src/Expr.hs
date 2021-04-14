{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Expr
(
    Expr5(..), zeroE, sumExpr2, LinearFunc5,
    Endpoint'(..), Edge'(..),
    AnyExpr(..)
)
where
import Prelude hiding ((.))
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector, scaleFunc, BasicVectors, negateFunc, identityFunc, BackFunc, LinearEdge)
import EType (Node(Node), Endpoint (InnerNode, SourceNode), Edge(Edge))
import Control.Category (Category(..))
import Data.Coerce (coerce)
import Data.Kind (Type)

data Endpoint' e da dv where
    SourceNode' :: Endpoint' e da da
    InnerNode' :: Expr5 e da dv -> Endpoint' e da dv

data Edge' e a v where
    Edge' :: e u v -> Endpoint' e a u -> Edge' e a v
    
data Expr5 e a v where
    Expr5Var :: Expr5 e a a
    Expr5 :: BasicVector v => [Edge' e a v] -> Expr5 e a v -- TODO: rename to Sum
    Expr5Subs :: Expr5 e x v -> Expr5 e a x -> Expr5 e a v

newtype AnyExpr e a v = AnyExpr (forall x. e v x -> Edge' e a x)

anyVar :: AnyExpr e a a
anyVar = AnyExpr (\f -> Edge' f SourceNode')

anyRelay :: Category e => e u v -> AnyExpr e a u -> AnyExpr e a v
anyRelay f (AnyExpr g) = AnyExpr (\x -> g (x . f))

realExpr :: Expr5 e a v -> AnyExpr e a v
realExpr x = AnyExpr (\f -> Edge' f (InnerNode' x))

--newtype LinearFunc5 e a v = LinearFunc5 (Endpoint' e a v)
-- TODO: remove LinearFunc5, use Expr5 everywhere
type LinearFunc5 = Endpoint'

instance Category (LinearFunc5 e) where
    id = SourceNode'
    x . y = (go x y)
        where go :: Endpoint' e b c -> Endpoint' e a b -> Endpoint' e a c
              go SourceNode' y' = y'
              go x' SourceNode'  = x'
              go (InnerNode' x') (InnerNode' y') = InnerNode' (Expr5Subs x' y')

zeroE :: BasicVector dv => Expr5 e da dv
zeroE = Expr5 []

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Expr5 e da dv) where
    zeroV = zeroE
    negateV x = Expr5 [Edge' negateFunc (InnerNode' x)]
    x ^+^ y = Expr5 [Edge' identityFunc (InnerNode' x), Edge' identityFunc (InnerNode' y)]
    x ^-^ y = Expr5 [Edge' identityFunc (InnerNode' x), Edge' negateFunc (InnerNode' y)]

instance FullVector dv => VectorSpace (Expr5 BackFunc da dv) where
    type Scalar (Expr5 BackFunc da dv) = Scalar dv
    a *^ v = Expr5 [Edge' (scaleFunc a) (InnerNode' v)]

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Endpoint' e da dv) where
    zeroV = InnerNode' zeroV
    negateV x = InnerNode' (Expr5 [Edge' negateFunc x])
    x ^+^ y = InnerNode' (Expr5 [Edge' identityFunc x, Edge' identityFunc y])
    x ^-^ y = InnerNode' (Expr5 [Edge' identityFunc x, Edge' negateFunc y])

instance FullVector dv => VectorSpace (Endpoint' BackFunc da dv) where
    type Scalar (Endpoint' BackFunc da dv) = Scalar (Expr5 BackFunc da dv)
    a *^ x = InnerNode' (Expr5 [Edge' (scaleFunc a) x])

sumExpr2 :: FullVector dv => [Expr5 BackFunc da dv] -> Expr5 BackFunc da dv
sumExpr2 xs = Expr5 (wrap <$> xs)
    where wrap x = Edge' identityFunc (InnerNode' x)
