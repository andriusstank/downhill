{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module Expr
(
    Expr(..), zeroE, zeroE', sumExpr2,
    Term(..),
    AnyExpr(..),
    anyVar, realExpr, castNode, sparseNode,
    SparseVector(..)
)
where
import Prelude hiding ((.))
import Data.VectorSpace (VectorSpace(..),  AdditiveGroup(..), sumV)
import Data.Constraint
import Notensor (VecBuilder, FullVector (negateBuilder, identityBuilder, scaleBuilder), BasicVector (sumBuilder, sumBuilder'), BackFun (BackFun))
import EType (Node(Node), Endpoint (InnerNode, SourceNode), Edge(Edge))
import Control.Category (Category(..))
import Data.Coerce (coerce)
import Data.Kind (Type)

data Term e a v where
    Term :: e u v -> Expr e a u -> Term e a v

data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v

newtype AnyExpr e a v = AnyExpr (forall x. e v x -> [Term e a x])

{-# ANN anyVar "HLint: ignore Avoid lambda using `infix`" #-}
anyVar :: AnyExpr e a a
anyVar = AnyExpr (\f -> [Term f ExprVar])

anyRelay :: Category e => e u v -> AnyExpr e a u -> AnyExpr e a v
anyRelay f (AnyExpr g) = AnyExpr (\x -> g (x . f))


{-# ANN realExpr "HLint: ignore Avoid lambda using `infix`" #-}
realExpr :: Expr e a v -> AnyExpr e a v
realExpr x = AnyExpr (\f -> [Term f x])

newtype SparseVector v = SparseVector { unSparseVector :: VecBuilder v }

deriving via (VecBuilder v) instance Semigroup (VecBuilder v) => Semigroup (SparseVector v)

instance Monoid (VecBuilder v) => BasicVector (SparseVector v) where
    type VecBuilder (SparseVector v) = VecBuilder v
    sumBuilder xs = SparseVector $ mconcat xs
    sumBuilder' = SparseVector

castNode
    :: forall a y v. (BasicVector y, VecBuilder v ~ VecBuilder y)
    => Expr BackFun a y -> AnyExpr BackFun a v
castNode node = AnyExpr go
    where go :: forall x. BackFun v x -> [Term BackFun a x]
          go (BackFun g) = [Term (BackFun g) node]

sparseNode
    :: forall a v. (BasicVector v)
    => Expr BackFun a (SparseVector v) -> AnyExpr BackFun a v
sparseNode = castNode

materialIdentity ::
    forall a v. (Monoid (VecBuilder v))
    => AnyExpr BackFun a v
    -> AnyExpr BackFun a v
materialIdentity = unarySparseFun unSparseVector

unarySparseFun ::
    forall a v. (Monoid (VecBuilder v))
    => (SparseVector v -> VecBuilder v)
    -> AnyExpr BackFun a v
    -> AnyExpr BackFun a v
unarySparseFun f (AnyExpr x) = castNode realNode
    where realNode :: Expr BackFun a (SparseVector v)
          realNode = ExprSum (x (BackFun f))

zeroE :: BasicVector dv => Expr e da dv
zeroE = ExprSum []

zeroE' :: AnyExpr e a v
zeroE' = AnyExpr (const [])

instance FullVector v => AdditiveGroup (Expr BackFun a v) where
    zeroV = zeroE
    negateV x = ExprSum [Term (BackFun negateBuilder) x]
    x ^+^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun identityBuilder) y]
    x ^-^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun negateBuilder) y]

instance FullVector dv => VectorSpace (Expr BackFun da dv) where
    type Scalar (Expr BackFun da dv) = Scalar dv
    a *^ v = ExprSum [Term (BackFun (scaleBuilder a)) v]

instance FullVector v => AdditiveGroup (AnyExpr BackFun a v) where
    zeroV = zeroE'
    negateV (AnyExpr x) = realExpr (ExprSum (x (BackFun negateBuilder)))
    AnyExpr x ^+^ AnyExpr y = realExpr (ExprSum (x (BackFun identityBuilder) <> y (BackFun identityBuilder)))
    AnyExpr x ^-^ AnyExpr y = realExpr (ExprSum (x (BackFun identityBuilder) <> y (BackFun negateBuilder)))

instance FullVector v => VectorSpace (AnyExpr BackFun a v) where
    type Scalar (AnyExpr BackFun a v) = Scalar v
    a *^ AnyExpr v = realExpr (ExprSum (v (BackFun (scaleBuilder a))))

sumExpr2 :: FullVector dv => [Expr BackFun da dv] -> Expr BackFun da dv
sumExpr2 xs = ExprSum (wrap <$> xs)
    where wrap x = Term (BackFun identityBuilder) x

