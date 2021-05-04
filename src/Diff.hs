{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Diff
(
    HasGrad(..),
    BVar(..), BVarS, bvarValue,
    constant, var,
    backprop, backpropS,
    fst, snd, zip
)
where

import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(AnyExpr), anyVar, realExpr, castNode, sparseNode, SparseVector (SparseVector), zeroE')
import Prelude hiding (fst, snd, zip)
import Affine (AffineFunc(AffineFunc))
import NodeMap (cvtmap, SomeSharedExprWithMap)
import qualified Graph
import qualified NodeMap
import System.IO.Unsafe (unsafePerformIO)
import Notensor (BasicVector(..), BackFun (BackFun), FullVector, flipBackFun)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.VectorSpace (AdditiveGroup(zeroV, (^+^)), Scalar)
import ExprWalker ()
import Graph (SomeGraph(SomeGraph), evalGraph)
import Data.Coerce (coerce, Coercible)
import OpenGraph (runRecoverSharing7, OpenGraph)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)

-- add `Scalar v ~ Scalar (GradOf v)` or not?
class (FullVector (GradOf v), Scalar (GradOf v) ~ Scalar v) => HasGrad v where
    type GradOf v :: Type
    evalGrad :: GradOf v -> v -> Scalar v

instance HasGrad Float where
    type GradOf Float = Float
    evalGrad = (*)

instance HasGrad Double where
    type GradOf Double = Double
    evalGrad = (*)

instance (Scalar u ~ Scalar v, AdditiveGroup (Scalar v), HasGrad u, HasGrad v) => HasGrad (u, v) where
    type GradOf (u, v) = (GradOf u, GradOf v)
    evalGrad (a, b) (x, y) = evalGrad a x ^+^ evalGrad b y

newtype BVar a v = BVar (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v)))

deriving via (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))) instance (Num v, HasGrad v, Scalar v ~ v) => Num (BVar a v)
deriving via (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))) instance (Fractional v, HasGrad v, Scalar v ~ v) => Fractional (BVar a v)
deriving via (AffineFunc v (AnyExpr BackFun (GradOf a) (GradOf v))) instance (Floating v, HasGrad v, Scalar v ~ v) => Floating (BVar a v)

type BVarS a = BVar a a

bvarValue :: BVar a v -> v
bvarValue (BVar (AffineFunc y0 _)) = y0

constant :: a -> BVar a a
constant x = BVar (AffineFunc x zeroE')

var :: b -> BVar b b
var x = BVar (AffineFunc x anyVar)

backpropNodeMap :: forall a z. BasicVector a => SomeSharedExprWithMap BackFun a z -> z -> a
backpropNodeMap m dv = case m of
    NodeMap.SomeSharedExprWithMap smap expr -> evalGraph fwdGraph dv
        where backGraph = Graph.Graph smap expr
              fwdGraph = Graph.flipGraph flipBackFun backGraph

backpropExpr :: forall da dv. (BasicVector da, FullVector dv) => AnyExpr BackFun da dv -> dv -> da
backpropExpr dy dv = unsafePerformIO $ do
    g <- runRecoverSharing7 dy
    return (backpropNodeMap (cvtmap g) dv)

backprop :: forall b a. (FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf b -> GradOf a
backprop (BVar (AffineFunc _y0 x)) = backpropExpr @(GradOf a) @(GradOf b) x

backpropS :: forall b a. (Num (GradOf b), FullVector (GradOf b), BasicVector (GradOf a)) => BVar a b -> GradOf a
backpropS x = backprop @b @a x 1

fst :: forall b1 b2 a. BasicVector (GradOf b1) => BVar a (b1, b2) -> BVar a b1
fst (BVar (AffineFunc (b1, _) (AnyExpr dv))) = BVar (AffineFunc b1 (sparseNode node))
    where f :: BackFun (GradOf (b1, b2)) (SparseVector (GradOf b1))
          f = BackFun (\(SparseVector x) -> (Just x, Nothing))
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1))
          node = ExprSum (dv f)

snd :: forall b1 b2 a. BasicVector (GradOf b2) => BVar a (b1, b2) -> BVar a b2
snd (BVar (AffineFunc (_, b2) (AnyExpr dv))) = BVar (AffineFunc b2 (sparseNode node))
    where f :: BackFun x (SparseVector (GradOf b2))
          f = BackFun (\(SparseVector x) -> (Nothing, Just x))
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b2))
          node = ExprSum (dv f)

zip :: forall b1 b2 a. (HasGrad b1, HasGrad b2) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip (BVar (AffineFunc b1 (AnyExpr db1))) (BVar (AffineFunc b2 (AnyExpr db2))) = BVar (AffineFunc (b1, b2) (sparseNode node))
    where f1 :: BackFun (GradOf b1) (SparseVector (GradOf (b1, b2)))
          f1 = BackFun (\(SparseVector (x, _)) -> fromMaybe mempty x)
          f2 :: BackFun (GradOf b2) (SparseVector (GradOf (b1, b2)))
          f2 = BackFun (\(SparseVector (_, y)) -> fromMaybe mempty y)
          node :: Expr BackFun (GradOf a) (SparseVector (GradOf b1, GradOf b2))
          node = ExprSum (db1 f1 <> db2 f2)
