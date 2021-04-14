{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Simplify where

import OpenMap(OpenMap, OpenKey)
import NodeMap (SomeNodeMap(..), uncheckedMakeNodeMap, NodeMap)
import NodeMap ()
import Graph(Graph (TrivialGraph, NonTrivialGraph), NonTrivialGraph(Graph), SomeGraph(SomeGraph))
import ExprWalker (CachedNode(..), Node''(..), NodeKey (InnerKey), Endpoint'' (SourceNode''), Edge''(..), CachedTree(CachedTree))

import qualified OpenMap
import qualified NodeMap
import Data.Kind (Type)
import Notensor (BasicVector)
import EType (Endpoint (SourceNode, InnerNode),Node (Node), Edge(Edge))
{-
data SimpleEndpoint s a v where
    SimpleSourceNode :: SimpleEndpoint s a a
    SimpleInnerNode :: NodeMap.NodeKey s v -> SimpleEndpoint s a v

data SimpleEdge s e a v where
    SimpleEdge :: e u v -> SimpleEndpoint s a u -> SimpleEdge s e a v

data SimpleNode s (e :: Type -> Type -> Type) a v where
    SimpleNode :: NodeMap.NodeKey s v -> SimpleNode s e a v
-}

data ReducedNode s a v where
    ReducedSourceNode :: ReducedNode s a a
    ReducedInnerNode :: NodeMap.NodeKey s v -> ReducedNode s a v


goB :: forall s e a z. BasicVector a => CachedTree e a z -> NodeMap s (Node'' e a) -> Graph s e a z
goB (CachedTree allNodes finalNode) innerNodes = NonTrivialGraph (Graph newInner (cvtNode finalNode))
    where reduce :: OpenMap (ReducedNode s a)
          reduce = OpenMap.mapmapWithKey go allNodes
            where go :: OpenKey v -> CachedNode e a v -> ReducedNode s a v
                  go key = \case
                    CachedSourceNode -> ReducedSourceNode
                    CachedClone (InnerKey node) -> case OpenMap.lookup reduce node of
                        Nothing -> error "bug: node not found"
                        Just x -> x
                    CachedInnerNode _ -> case NodeMap.tryLookup innerNodes key of
                        Nothing -> error "bug: inner node not found"
                        Just (skey, _) -> ReducedInnerNode skey
          cvtEndpoint :: NodeKey a v -> Endpoint (NodeMap.NodeKey s) a v
          cvtEndpoint (InnerKey key) = case OpenMap.lookup reduce key of
              Nothing -> error "node not found"
              Just node -> case node of
                  ReducedSourceNode -> SourceNode
                  ReducedInnerNode skey -> InnerNode skey
          cvtEdge :: forall x. Edge'' e a x -> Edge (NodeMap.NodeKey s) e a x
          cvtEdge (Edge'' f x) = Edge f (cvtEndpoint x)

          cvtNode :: forall x. Node'' e a x -> Node (NodeMap.NodeKey s) e a x
          cvtNode = \case
            Node'' edges -> Node (map cvtEdge edges)
        
          newInner :: NodeMap s (Node (NodeMap.NodeKey s) e a)
          newInner = NodeMap.mapmap cvtNode innerNodes

goA :: forall e a z. BasicVector a => CachedTree e a z -> SomeGraph e a z
goA tree@(CachedTree cnodes finalNode) =
    case innerNodes of
        SomeNodeMap innerNodes' -> case allNodes of
            SomeNodeMap allNodes' -> SomeGraph (goB tree innerNodes')
    where innerNodes :: SomeNodeMap (Node'' e a)
          innerNodes = uncheckedMakeNodeMap (OpenMap.mapmapMaybe go cnodes)
            where go :: forall x. CachedNode e a x -> Maybe (Node'' e a x)
                  go = \case
                    CachedSourceNode -> Nothing
                    CachedInnerNode x -> Just x
                    CachedClone _ -> Nothing
                    CachedCoerce _ -> Nothing
          allNodes :: SomeNodeMap (CachedNode e a)
          allNodes = uncheckedMakeNodeMap cnodes
