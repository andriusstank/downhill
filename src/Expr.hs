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
    Expr(..), zeroE, sumExpr2,
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
    Term :: e u v -> Expr e a u -> Term e a v

data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v

newtype AnyExpr e a v = AnyExpr (forall x. e v x -> Term e a x)

anyVar :: AnyExpr e a a
anyVar = AnyExpr (\f -> Term f ExprVar)

anyRelay :: Category e => e u v -> AnyExpr e a u -> AnyExpr e a v
anyRelay f (AnyExpr g) = AnyExpr (\x -> g (x . f))

realExpr :: Expr e a v -> AnyExpr e a v
realExpr x = AnyExpr (\f -> Term f x)

zeroE :: BasicVector dv => Expr e da dv
zeroE = ExprSum []

instance (LinearEdge e, FullVector dv) => AdditiveGroup (Expr e da dv) where
    zeroV = zeroE
    negateV x = ExprSum [Term negateFunc x]
    x ^+^ y = ExprSum [Term identityFunc x, Term identityFunc y]
    x ^-^ y = ExprSum [Term identityFunc x, Term negateFunc y]

instance FullVector dv => VectorSpace (Expr BackFunc da dv) where
    type Scalar (Expr BackFunc da dv) = Scalar dv
    a *^ v = ExprSum [Term (scaleFunc a) v]

sumExpr2 :: FullVector dv => [Expr BackFunc da dv] -> Expr BackFunc da dv
sumExpr2 xs = ExprSum (wrap <$> xs)
    where wrap x = Term identityFunc x
