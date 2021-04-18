{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Expr
(
    Expr5(..), zeroE, sumExpr2, LinearFunc5,
    Term(..),
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

data Term e a v where
    Term :: e u v -> Expr5 e a u -> Term e a v
    
data Expr5 e a v where
    Expr5Var :: Expr5 e a a
    Expr5 :: BasicVector v => [Term e a v] -> Expr5 e a v -- TODO: rename to Sum

newtype AnyExpr e a v = AnyExpr (forall x. e v x -> Term e a x)

anyVar :: AnyExpr e a a
anyVar = AnyExpr (\f -> Term f Expr5Var)

anyRelay :: Category e => e u v -> AnyExpr e a u -> AnyExpr e a v
anyRelay f (AnyExpr g) = AnyExpr (\x -> g (x . f))

realExpr :: Expr5 e a v -> AnyExpr e a v
realExpr x = AnyExpr (\f -> Term f x)

--newtype LinearFunc5 e a v = LinearFunc5 (Endpoint' e a v)
-- TODO: remove LinearFunc5, use Expr5 everywhere
type LinearFunc5 = Expr5

zeroE :: BasicVector dv => Expr5 e da dv
zeroE = Expr5 []

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Expr5 e da dv) where
    zeroV = zeroE
    negateV x = Expr5 [Term negateFunc x]
    x ^+^ y = Expr5 [Term identityFunc x, Term identityFunc y]
    x ^-^ y = Expr5 [Term identityFunc x, Term negateFunc y]

instance FullVector dv => VectorSpace (Expr5 BackFunc da dv) where
    type Scalar (Expr5 BackFunc da dv) = Scalar dv
    a *^ v = Expr5 [Term (scaleFunc a) v]

sumExpr2 :: FullVector dv => [Expr5 BackFunc da dv] -> Expr5 BackFunc da dv
sumExpr2 xs = Expr5 (wrap <$> xs)
    where wrap x = Term identityFunc x
