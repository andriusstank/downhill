{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Internal.Graph.Graph (
  -- * Graph type
  Graph (..),
  Node (..),
  Edge (..),
  Endpoint (..),
  SomeGraph (..),
  mapGraph,

  -- * Evaluate

  -- evalGraph,

  -- * Transpose
  transposeGraph,
  -- transposeFwdGraph,
  -- transposeBackGraph,

  -- * Construct
  unsafeFromOpenGraph,

  -- * Internal
  allGraphEdges,
  edgeListToGraph,
  AnyEdge (..),
  graphNodes,
  NodeDict (..),
  flipAnyEdge,
)
where

import Data.Either (partitionEithers)
import Data.Kind (Type)
import Downhill.Internal.Graph.NodeMap (
  IsNodeSet,
  KeyAndValue (KeyAndValue),
  NodeKey,
  NodeMap,
  SomeNodeMap (SomeNodeMap),
 )
import Downhill.Internal.Graph.NodeMap qualified as NodeMap
import Downhill.Internal.Graph.OpenGraph (OpenEdge (OpenEdge), OpenEndpoint (OpenInnerNode, OpenSourceNode), OpenGraph (OpenGraph), OpenNode (OpenNode))
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Expr (BiFun, GradType, TangType, VecPair)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Prelude hiding (head, tail)

data Endpoint s a v where
  SourceNode :: Endpoint s a a
  InnerNode :: NodeKey s v -> Endpoint s a v

type Edge :: forall {k}. Type -> (k -> k -> Type) -> k -> k -> Type
data Edge s e a v where
  Edge :: e u v -> Endpoint s a u -> Edge s e a v

{- | Inner node. This does not include initial node. Contains a list
of ingoing edges.
-}
type Node :: Type -> (VecPair -> VecPair -> Type) -> VecPair -> VecPair -> Type
data Node s e a v = (HasBuilder (TangType v), HasBuilder (GradType v)) => Node [Edge s e a v]

type Graph :: Type -> (VecPair -> VecPair -> Type) -> VecPair -> VecPair -> Type
data Graph s e a z
  = (HasBuilder (TangType a), HasBuilder (GradType a)) =>
  Graph
  { graphInnerNodes :: NodeMap s (Node s e a)
  , graphFinalNode :: Node s e a z
  }

data SomeGraph e a z where
  SomeGraph ::
    forall s (e :: VecPair -> VecPair -> Type) (a :: VecPair) (z :: VecPair).
    (IsNodeSet s VecPair) =>
    Graph s e a z -> SomeGraph e a z

{- `Edge` stores head endpoint only. `AnyEdge` stores both endpoints. -}
data AnyEdge s e a z
  = forall u v.
  AnyEdge
  { _edgeTail :: Endpoint s z v
  , _edgeLabel :: e u v
  , _edgeHead :: Endpoint s a u
  }

nodeEdges :: forall s f a z x. NodeKey s x -> Node s f a x -> [AnyEdge s f a z]
nodeEdges name (Node xs) = go <$> xs
 where
  go :: Edge s f a x -> AnyEdge s f a z
  go (Edge f head) = AnyEdge (InnerNode name) f head

allGraphEdges :: forall s f a z. Graph s f a z -> [AnyEdge s f a z]
allGraphEdges (Graph innerNodes (Node es)) = finalEdges ++ innerEdges
 where
  innerEdges :: [AnyEdge s f a z]
  innerEdges = concat (NodeMap.toListWith nodeEdges innerNodes)
  finalEdges :: [AnyEdge s f a z]
  finalEdges = wrapFinalEdge <$> es
   where
    wrapFinalEdge :: Edge s f a z -> AnyEdge s f a z
    wrapFinalEdge (Edge f head) = AnyEdge SourceNode f head

sortByTail ::
  forall s f da dz.
  AnyEdge s f da dz ->
  Either (Edge s f da dz) (KeyAndValue s (Edge s f da))
sortByTail (AnyEdge tail f head) = case tail of
  SourceNode -> Left (Edge f head)
  InnerNode x -> Right (KeyAndValue x (Edge f head))

flipAnyEdge :: (forall u v. f u v -> g v u) -> AnyEdge s f a z -> AnyEdge s g z a
flipAnyEdge flipF (AnyEdge tail f head) = AnyEdge head (flipF f) tail

{- BasicVector constraint is needed to construct a node.
   `NodeMap s NodeDict` is a list of all nodes.
-}
data NodeDict x = (HasBuilder (TangType x), HasBuilder (GradType x)) => NodeDict

emptyNodeMap :: forall s e z. NodeMap s (NodeDict :: VecPair -> Type) -> NodeMap s (Node s e z)
emptyNodeMap = NodeMap.map emptyNode
 where
  emptyNode :: forall x. NodeDict x -> Node s e z x
  emptyNode = \case
    NodeDict -> Node []

edgeListToGraph ::
  forall s e a z.
  ( IsNodeSet s VecPair
  , HasBuilder (TangType a)
  , HasBuilder (GradType a)
  , HasBuilder (TangType z)
  , HasBuilder (GradType z)
  ) =>
  NodeMap s (NodeDict :: VecPair -> Type) ->
  [AnyEdge s e z a] ->
  Graph s e z a
edgeListToGraph nodes flippedEdges = Graph innerNodes (Node initialEdges)
 where
  initialEdges :: [Edge s e z a]
  innerEdges :: [KeyAndValue s (Edge s e z)]
  (initialEdges, innerEdges) = partitionEithers (sortByTail <$> flippedEdges)
  prependToMap :: KeyAndValue s (Edge s e z) -> NodeMap s (Node s e z) -> NodeMap s (Node s e z)
  prependToMap (KeyAndValue key edge) = NodeMap.adjust prependToNode key
   where
    prependToNode (Node edges) = Node (edge : edges)
  innerNodes = foldr prependToMap (emptyNodeMap nodes) innerEdges

graphNodes :: forall s f da dz. Graph s f da dz -> NodeMap s (NodeDict :: VecPair -> Type)
graphNodes (Graph env _) = NodeMap.map go env
 where
  go :: Node s f da dv -> NodeDict dv
  go = \case
    Node _ -> NodeDict

-- | Reverse edges. Turns reverse mode evaluation into forward mode.
transposeGraph ::
  forall s f g a z.
  (IsNodeSet s VecPair) =>
  (forall u v. f u v -> g v u) -> Graph s f a z -> Graph s g z a
transposeGraph flipEdge g@(Graph _ (Node _)) = edgeListToGraph @s (graphNodes g) flippedEdges
 where
  edges :: [AnyEdge s f a z]
  edges = allGraphEdges g
  flippedEdges :: [AnyEdge s g z a]
  flippedEdges = flipAnyEdge flipEdge <$> edges

_mapEdges :: forall s f g a z. (forall u v. f u v -> g u v) -> Graph s f a z -> Graph s g a z
_mapEdges f (Graph inner final) = Graph (NodeMap.map go inner) (go final)
 where
  go :: Node s f a v -> Node s g a v
  go (Node xs) = Node [goEdge x | x <- xs]
  goEdge :: Edge p f a x -> Edge p g a x
  goEdge (Edge e x) = Edge (f e) x

unsafeConstructGraph ::
  forall s a v.
  ( IsNodeSet s VecPair
  , HasCallStack
  , HasBuilder (TangType a)
  , HasBuilder (GradType a)
  ) =>
  NodeMap s (OpenNode a) -> OpenNode a v -> Graph s BiFun a v
unsafeConstructGraph m x = Graph (NodeMap.map mkExpr m) (mkExpr x)
 where
  mkExpr :: forall x. OpenNode a x -> Node s BiFun a x
  mkExpr = \case
    OpenNode terms -> Node (mkTerm <$> terms)
  mkTerm :: forall x. OpenEdge a x -> Edge s BiFun a x
  mkTerm = \case
    OpenEdge f x' -> Edge f (mkArg x')
  mkArg :: forall u. OpenEndpoint a u -> Endpoint s a u
  mkArg = \case
    OpenSourceNode -> SourceNode
    OpenInnerNode key -> case NodeMap.tryLookup m key of
      Just (key', _value) -> InnerNode key'
      Nothing -> error ("Downhill: invalid key in constructGraph\n" ++ prettyCallStack callStack)

-- | Will crash if graph has invalid keys
unsafeFromOpenGraph ::
  forall (a :: VecPair) (v :: VecPair).
  ( HasCallStack
  , HasBuilder (TangType a)
  , HasBuilder (GradType a)
  ) =>
  OpenGraph a v -> SomeGraph BiFun a v
unsafeFromOpenGraph (OpenGraph x m) =
  case NodeMap.fromOpenMap m of
    SomeNodeMap m' -> SomeGraph (unsafeConstructGraph m' x)

-- data Node s e a v = (NodeConstraint2 v) => Node [Edge s e a v]
mapNode :: forall s e1 e2 a x. (forall u v. e1 u v -> e2 u v) -> Node s e1 a x -> Node s e2 a x
mapNode f (Node edges) = Node (map mapEdge edges)
 where
  mapEdge :: Edge s e1 a x -> Edge s e2 a x
  mapEdge (Edge e x) = Edge (f e) x

mapGraph :: forall s e1 e2 a z. (forall u v. e1 u v -> e2 u v) -> Graph s e1 a z -> Graph s e2 a z
mapGraph f (Graph nodes finalNode) = Graph (NodeMap.map go nodes) (go finalNode)
 where
  go :: forall v. Node s e1 a v -> Node s e2 a v
  go = mapNode f
