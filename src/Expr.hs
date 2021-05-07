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
import Notensor (VecBuilder, FullVector (negateBuilder, identityBuilder, scaleBuilder), BasicVector (sumBuilder'), BackFun (BackFun))
import EType (Node(Node), Endpoint (InnerNode, SourceNode), Edge(Edge))
import Control.Category (Category(..))
import Data.Coerce (coerce)
import Data.Kind (Type)

data Term e a v where
    Term :: e u v -> Expr e a u -> Term e a v

data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v

newtype AnyExpr a v = AnyExpr (forall x. (x -> VecBuilder v) -> [Term BackFun a x])

{-# ANN anyVar "HLint: ignore Avoid lambda using `infix`" #-}
anyVar :: AnyExpr a a
anyVar = AnyExpr (\f -> [Term (BackFun f) ExprVar])

{-# ANN realExpr "HLint: ignore Avoid lambda using `infix`" #-}
realExpr :: Expr BackFun a v -> AnyExpr a v
realExpr x = AnyExpr (\f -> [Term (BackFun f) x])

newtype SparseVector v = SparseVector { unSparseVector :: VecBuilder v }

deriving via (VecBuilder v) instance Semigroup (VecBuilder v) => Semigroup (SparseVector v)

instance Monoid (VecBuilder v) => BasicVector (SparseVector v) where
    type VecBuilder (SparseVector v) = VecBuilder v
    sumBuilder' = SparseVector

castNode
    :: forall a y v. (BasicVector y, VecBuilder v ~ VecBuilder y)
    => Expr BackFun a y -> AnyExpr a v
castNode node = AnyExpr go
    where go :: forall x. (x -> VecBuilder v) -> [Term BackFun a x]
          go g = [Term (BackFun g) node]

sparseNode
    :: forall a v. (BasicVector v)
    => Expr BackFun a (SparseVector v) -> AnyExpr a v
sparseNode = castNode

materialIdentity ::
    forall a v. (Monoid (VecBuilder v))
    => AnyExpr a v
    -> AnyExpr a v
materialIdentity = unarySparseFun unSparseVector

unarySparseFun ::
    forall a v. (Monoid (VecBuilder v))
    => (SparseVector v -> VecBuilder v)
    -> AnyExpr a v
    -> AnyExpr a v
unarySparseFun f (AnyExpr x) = castNode realNode
    where realNode :: Expr BackFun a (SparseVector v)
          realNode = ExprSum (x f)

zeroE :: BasicVector dv => Expr e da dv
zeroE = ExprSum []

zeroE' :: AnyExpr a v
zeroE' = AnyExpr (const [])

instance FullVector v => AdditiveGroup (Expr BackFun a v) where
    zeroV = zeroE
    negateV x = ExprSum [Term (BackFun negateBuilder) x]
    x ^+^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun identityBuilder) y]
    x ^-^ y = ExprSum [Term (BackFun identityBuilder) x, Term (BackFun negateBuilder) y]

instance FullVector dv => VectorSpace (Expr BackFun da dv) where
    type Scalar (Expr BackFun da dv) = Scalar dv
    a *^ v = ExprSum [Term (BackFun (scaleBuilder a)) v]

instance FullVector v => AdditiveGroup (AnyExpr a v) where
    zeroV = zeroE'
    negateV (AnyExpr x) = realExpr (ExprSum (x negateBuilder))
    AnyExpr x ^+^ AnyExpr y = realExpr (ExprSum (x identityBuilder <> y identityBuilder))
    AnyExpr x ^-^ AnyExpr y = realExpr (ExprSum (x identityBuilder <> y negateBuilder))

instance FullVector v => VectorSpace (AnyExpr a v) where
    type Scalar (AnyExpr a v) = Scalar v
    a *^ AnyExpr v = realExpr (ExprSum (v (scaleBuilder a)))

sumExpr2 :: FullVector dv => [Expr BackFun da dv] -> Expr BackFun da dv
sumExpr2 xs = ExprSum (wrap <$> xs)
    where wrap x = Term (BackFun identityBuilder) x

