{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Internal.Graph.Graph
  ( Graph (..),
    SomeGraph (..),
    evalGraph,
    transposeGraph,
    fromOpenGraph,
  )
where

import Data.Either (partitionEithers)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Downhill.Internal.Graph.NodeMap
  ( IsNodeSet,
    NodeKey,
    NodeMap,
    KeyAndValue (KeyAndValue),
    SomeNodeMap (SomeNodeMap),
  )
import qualified Downhill.Internal.Graph.NodeMap as NodeMap
import Downhill.Internal.Graph.OpenGraph (OpenExpr, OpenGraph (OpenGraph))
import Downhill.Internal.Graph.OpenMap (OpenKey)
import Downhill.Internal.Graph.Types (Edge (..), Endpoint (InnerNode, SourceNode), FwdFun (FwdFun), Node (Node))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Prelude hiding (head, tail)

-- | A set of inner nodes plus a final node.
-- Initial node is special – it has no incoming edges. Final node becomes initial node
-- when the edges are flipped, hence it's also special and needs to be stored separately.
data Graph s e a z = BasicVector a =>
  Graph
  { graphInnerNodes :: NodeMap s (Node (NodeKey s) e a),
    graphFinalNode :: Node (NodeKey s) e a z
  }

data SomeGraph e a z where
  SomeGraph :: IsNodeSet s => Graph s e a z -> SomeGraph e a z

{- `Edge` stores head endpoint only. `AnyEdge` stores both endpoints. -}
data AnyEdge s e a z = forall u v.
  AnyEdge
  { _edgeTail :: Endpoint (NodeKey s) z v,
    _edgeLabel :: e u v,
    _edgeHead :: Endpoint (NodeKey s) a u
  }

-- | Forward mode evaluation
evalGraph :: forall s x z. Graph s FwdFun z x -> z -> x
evalGraph (Graph nodes finalNode) dz = evalNode finalNode
  where
    evalParent :: forall v. Endpoint (NodeKey s) z v -> v
    evalParent = \case
      SourceNode -> dz
      InnerNode nodeName -> runIdentity (NodeMap.lookup innerValues nodeName)
    evalEdge :: Edge (NodeKey s) FwdFun z v -> VecBuilder v
    evalEdge (Edge (FwdFun f) tail) = f $ evalParent tail
    evalNode :: Node (NodeKey s) FwdFun z v -> v
    evalNode (Node xs) = sumBuilder (mconcat [evalEdge x | x <- xs])
    innerValues :: NodeMap s Identity
    innerValues = NodeMap.map (Identity . evalNode) nodes

nodeEdges :: forall s f a z x. NodeKey s x -> Node (NodeKey s) f a x -> [AnyEdge s f a z]
nodeEdges name (Node xs) = go <$> xs
  where
    go :: Edge (NodeKey s) f a x -> AnyEdge s f a z
    go (Edge f head) = AnyEdge (InnerNode name) f head

allGraphEdges :: forall s f a z. Graph s f a z -> [AnyEdge s f a z]
allGraphEdges (Graph innerNodes (Node es)) = finalEdges ++ innerEdges
  where
    innerEdges :: [AnyEdge s f a z]
    innerEdges = concat (NodeMap.toListWith nodeEdges innerNodes)
    finalEdges :: [AnyEdge s f a z]
    finalEdges = wrapFinalEdge <$> es
      where
        wrapFinalEdge :: Edge (NodeKey s) f a z -> AnyEdge s f a z
        wrapFinalEdge (Edge f head) = AnyEdge SourceNode f head

sortByTail ::
  forall s f da dz.
  AnyEdge s f da dz ->
  Either (Edge (NodeKey s) f da dz) (KeyAndValue s (Edge (NodeKey s) f da))
sortByTail (AnyEdge tail f head) = case tail of
  SourceNode -> Left (Edge f head)
  InnerNode x -> Right (KeyAndValue x (Edge f head))

flipAnyEdge :: (forall u v. f u v -> g v u) -> AnyEdge s f a z -> AnyEdge s g z a
flipAnyEdge flipF (AnyEdge tail f head) = AnyEdge head (flipF f) tail

{- BasicVector constraint is needed to construct a node.
   `NodeMap s NodeDict` is a list of all nodes.
-}
data NodeDict x = BasicVector x => NodeDict

emptyNodeMap :: forall s e z. NodeMap s NodeDict -> NodeMap s (Node (NodeKey s) e z)
emptyNodeMap = NodeMap.map emptyNode
  where
    emptyNode :: forall x. NodeDict x -> Node (NodeKey s) e z x
    emptyNode = \case
      NodeDict -> Node []

edgeListToGraph ::
  forall s e a z.
  (IsNodeSet s, BasicVector a, BasicVector z) =>
  NodeMap s NodeDict ->
  [AnyEdge s e z a] ->
  Graph s e z a
edgeListToGraph nodes flippedEdges = Graph innerNodes (Node initialEdges)
  where
    initialEdges :: [Edge (NodeKey s) e z a]
    innerEdges :: [KeyAndValue s (Edge (NodeKey s) e z)]
    (initialEdges, innerEdges) = partitionEithers (sortByTail <$> flippedEdges)
    prependToMap :: KeyAndValue s (Edge (NodeKey s) e z) -> NodeMap s (Node (NodeKey s) e z) -> NodeMap s (Node (NodeKey s) e z)
    prependToMap (KeyAndValue key edge) = NodeMap.adjust prependToNode key
      where
        prependToNode (Node edges) = Node (edge : edges)
    innerNodes = foldr prependToMap (emptyNodeMap nodes) innerEdges

backFromEdges ::
  forall s f g da dz.
  (IsNodeSet s, BasicVector da, BasicVector dz) =>
  (forall u v. f u v -> g v u) ->
  NodeMap s NodeDict ->
  [AnyEdge s f da dz] ->
  Graph s g dz da
backFromEdges flipFunc dictmap edges = edgeListToGraph dictmap flippedEdges
  where
    flippedEdges :: [AnyEdge s g dz da]
    flippedEdges = flipAnyEdge flipFunc <$> edges

graphNodes :: Graph s f da dz -> NodeMap s NodeDict
graphNodes (Graph env _) = NodeMap.map go env
  where
    go :: Node (NodeKey s) f da dv -> NodeDict dv
    go = \case
      Node _ -> NodeDict

-- | Reverse edges. Turns reverse mode evaluation into forward mode. |
transposeGraph :: IsNodeSet s => (forall u v. f u v -> g v u) -> Graph s f a z -> Graph s g z a
transposeGraph flipEdge g@(Graph _ (Node _)) = backFromEdges flipEdge (graphNodes g) (allGraphEdges g)

_mapEdges :: forall s f g da dz. (forall u v. f u v -> g u v) -> Graph s f da dz -> Graph s g da dz
_mapEdges f (Graph inner final) = Graph (NodeMap.map go inner) (go final)
  where
    go :: Node (NodeKey s) f da dv -> Node (NodeKey s) g da dv
    go (Node xs) = Node [goEdge x | x <- xs]
    goEdge :: Edge p f da dx -> Edge p g da dx
    goEdge (Edge e x) = Edge (f e) x

cvthelper :: forall s e a v. (IsNodeSet s, BasicVector a) => NodeMap s (OpenExpr e a) -> Node OpenKey e a v -> SomeGraph e a v
cvthelper m x = SomeGraph (Graph (NodeMap.map cvtexpr m) (cvtexpr x))
  where
    cvtexpr :: forall x. OpenExpr e a x -> Node (NodeKey s) e a x
    cvtexpr = \case
      Node terms -> Node (cvtterm <$> terms)
    cvtterm :: forall x. Edge OpenKey e a x -> Edge (NodeKey s) e a x
    cvtterm = \case
      Edge f x' -> Edge f (cvtarg x')
    cvtarg :: forall u. Endpoint OpenKey a u -> Endpoint (NodeKey s) a u
    cvtarg = \case
      SourceNode -> SourceNode
      InnerNode key -> case NodeMap.tryLookup m key of
        Just (key', _value) -> InnerNode key'
        Nothing -> error "oh fuck"

fromOpenGraph :: BasicVector a => OpenGraph e a v -> SomeGraph e a v
fromOpenGraph (OpenGraph x m) =
  case NodeMap.fromOpenMap m of
    SomeNodeMap m' -> cvthelper m' x
