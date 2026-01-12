{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Internal.Graph.Graph (
  Graph (..),
  FwdFun (..),
  BackFun (..),
  buildBigraph,
  DenseBigraph(..)
)
where

import Data.Kind (Type)
import Downhill.Internal.Graph.UniGraph (
  Edge (..),
  Endpoint (..),
  UniGraph (..),
  Node (..),
  SomeGraph (..),
  mapGraph,
  transposeGraph,
 )
import Downhill.Internal.Graph.UniGraph qualified as UniGraph
import Downhill.Internal.Graph.NodeMap (IsNodeSet, NodeMap)
import Downhill.Internal.Graph.NodeMap qualified as NodeMap
import Downhill.Internal.Graph.OpenGraph (recoverSharing)
import Downhill.Linear.Builder (HasBuilder (..), Dense (Dense, unDense))
import Downhill.Linear.Expr (
  BiFun (..),
  GradType,
  TangType,
  Term,
  VecPair (VecPair),
 )
import Downhill.Linear.Fun (DualOf, LinearFun (..))
import GHC.IO.Unsafe (unsafePerformIO)
import Prelude hiding (tail)

buildGraph ::
  forall (a :: VecPair) (v :: VecPair).
  ( HasBuilder (TangType a)
  , HasBuilder (GradType a)
  , HasBuilder (TangType v)
  , HasBuilder (GradType v)
  ) =>
  [Term a v] ->
  IO (SomeGraph BiFun a v)
buildGraph fidentityBuilder = do
  og <- recoverSharing fidentityBuilder
  return (UniGraph.unsafeFromOpenGraph og)

newtype FwdFun u v = FwdFun2 {_unFwdFun :: TangType u -> VecBuilder (TangType v)}

-- | Edge type for backward mode evaluation
newtype BackFun v u = BackFun2 {_unBackFun :: GradType v -> VecBuilder (GradType u)}

type GraphPair :: Type -> VecPair -> VecPair -> Type
data GraphPair s a z = GraphPair (UniGraph s FwdFun a z) (UniGraph s BackFun z a)

data Graph a z where
  Graph ::
    forall s a z.
    (IsNodeSet s VecPair) =>
    UniGraph s FwdFun ('VecPair a (DualOf a)) ('VecPair z (DualOf z)) ->
    UniGraph s BackFun ('VecPair z (DualOf z)) ('VecPair a (DualOf a)) ->
    Graph a z

buildGraphPair :: (IsNodeSet s VecPair) => UniGraph s BiFun a z -> GraphPair s a z
buildGraphPair graph = GraphPair fwd' back'
 where
  fwd' = mapGraph mkFwdFun graph
  back' = transposeGraph mkBackFun graph

  mkFwdFun :: BiFun u v -> FwdFun u v
  mkFwdFun x = FwdFun2 x.fwd
  mkBackFun :: BiFun u v -> BackFun v u
  mkBackFun x = BackFun2 x.back

buildBigraph ::
  forall a z.
  ( HasBuilder z
  , HasBuilder a
  , HasBuilder (DualOf z)
  , HasBuilder (DualOf a)
  ) =>
  [Term ('VecPair a (DualOf a)) ('VecPair z (DualOf z))] ->
  Graph a z
buildBigraph x =
  case unsafePerformIO (buildGraph x) of
    SomeGraph g2 -> case buildGraphPair g2 of
      GraphPair fwd' back' -> Graph fwd' back'

newtype VecHolder v = VecHolder {unVecHolder :: TangType v}
newtype CovHolder v = CovHolder {unCovHolder :: GradType v}

-- | Forward mode evaluation
evalGraph2 ::
  forall s x z.
  UniGraph s FwdFun z x -> TangType z -> TangType x
evalGraph2 (UniGraph nodes finalNode) dz = evalNode finalNode
 where
  evalParent :: forall v. Endpoint s z v -> TangType v
  evalParent = \case
    SourceNode -> dz
    InnerNode nodeName -> unVecHolder (NodeMap.lookup innerValues nodeName)
  evalEdge :: Edge s FwdFun z v -> VecBuilder (TangType v)
  evalEdge (Edge (FwdFun2 f) tail) = f $ evalParent tail
  evalNode :: forall (v :: VecPair). Node s FwdFun z v -> TangType v
  evalNode (Node xs) = sumBuilder (mconcat [evalEdge x | x <- xs])

  innerValues :: NodeMap s VecHolder
  innerValues = NodeMap.map (VecHolder . evalNode) nodes

-- | Reverse mode evaluation
evalGraph3 ::
  forall s x z.
  UniGraph s BackFun z x -> GradType z -> GradType x
evalGraph3 (UniGraph nodes finalNode) dz = evalNode finalNode
 where
  evalParent :: forall v. Endpoint s z v -> GradType v
  evalParent = \case
    SourceNode -> dz
    InnerNode nodeName -> unCovHolder (NodeMap.lookup innerValues nodeName)
  evalEdge :: Edge s BackFun z v -> VecBuilder (GradType v)
  evalEdge (Edge (BackFun2 f) tail) = f $ evalParent tail
  evalNode :: forall (v :: VecPair). Node s BackFun z v -> GradType v
  evalNode (Node xs) = sumBuilder (mconcat [evalEdge x | x <- xs])

  innerValues :: NodeMap s CovHolder
  innerValues = NodeMap.map (CovHolder . evalNode) nodes

instance (a ~ DualOf (DualOf a), z ~ DualOf (DualOf z), a ~ a', z ~ z') => LinearFun (Graph a z) a' z' where
  Graph fwd' _back <*| x = evalGraph2 fwd' x
  x |*> Graph _fwd back' = evalGraph3 back' x

newtype DenseBigraph a z = DenseBigraph { unDenseBigraph :: Graph (Dense a) (Dense z) }

instance (a ~ DualOf (DualOf a), z ~ DualOf (DualOf z)) => LinearFun (DenseBigraph a z) a z where
  DenseBigraph f <*| x = unDense (f <*| Dense x)
  x |*> DenseBigraph f = unDense (Dense x |*> f)
