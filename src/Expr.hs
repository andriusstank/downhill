{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module Expr
(
    Expr(..), zeroE, sumExpr2,
    Term(..),
    AnyExpr(..),
    anyVar, realExpr, castNode,
    SparseVector(..)
)
where
import Prelude hiding ((.))
import Tensor
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector, FullVectors, BasicVector (sumBuilder), scaleFunc, BasicVectors, negateFunc, identityFunc, BackFunc (BackFunc), LinearEdge)
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

{-# ANN anyVar "HLint: ignore Avoid lambda using `infix`" #-}
anyVar :: AnyExpr e a a
anyVar = AnyExpr (\f -> Term f ExprVar)

anyRelay :: Category e => e u v -> AnyExpr e a u -> AnyExpr e a v
anyRelay f (AnyExpr g) = AnyExpr (\x -> g (x . f))


{-# ANN realExpr "HLint: ignore Avoid lambda using `infix`" #-}
realExpr :: Expr e a v -> AnyExpr e a v
realExpr x = AnyExpr (\f -> Term f x)

newtype SparseVector v = SparseVector { unSparseVector :: VecBuilder v }

deriving via (VecBuilder v) instance Semigroup (VecBuilder v) => Semigroup (SparseVector v)

instance Monoid (VecBuilder v) => BasicVector (SparseVector v) where
    type VecBuilder (SparseVector v) = VecBuilder v
    sumBuilder xs = SparseVector $ mconcat xs

castNode
    :: forall a y v. (BasicVector y, VecBuilder v ~ VecBuilder y)
    => Expr BackFunc a y -> AnyExpr BackFunc a v
castNode node = AnyExpr go
    where go :: forall x. BackFunc v x -> Term BackFunc a x
          go (BackFunc g) = Term (BackFunc g) node

materialIdentity ::
    forall a v. (Monoid (VecBuilder v))
    => AnyExpr BackFunc a v
    -> AnyExpr BackFunc a v
materialIdentity = unarySparseFun unSparseVector

unarySparseFun ::
    forall a v. (Monoid (VecBuilder v))
    => (SparseVector v -> VecBuilder v)
    -> AnyExpr BackFunc a v
    -> AnyExpr BackFunc a v
unarySparseFun f (AnyExpr x) = castNode realNode
    where realNode :: Expr BackFunc a (SparseVector v)
          realNode = ExprSum [x (BackFunc f)]

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

instance (LinearEdge e, FullVector dv) => AdditiveGroup (AnyExpr e da dv) where
    zeroV = realExpr zeroE
    negateV (AnyExpr x) = realExpr (ExprSum [x negateFunc])
    AnyExpr x ^+^ AnyExpr y = realExpr (ExprSum [x identityFunc, y identityFunc])
    AnyExpr x ^-^ AnyExpr y = realExpr (ExprSum [x identityFunc, y negateFunc])

instance FullVector dv => VectorSpace (AnyExpr BackFunc da dv) where
    type Scalar (AnyExpr BackFunc da dv) = Scalar dv
    a *^ AnyExpr v = realExpr (ExprSum [v (scaleFunc a)])

sumExpr2 :: FullVector dv => [Expr BackFunc da dv] -> Expr BackFunc da dv
sumExpr2 xs = ExprSum (wrap <$> xs)
    where wrap x = Term identityFunc x

