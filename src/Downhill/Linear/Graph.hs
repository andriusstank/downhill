{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}

module Downhill.Linear.Graph
    ( Graph(..), SomeGraph(..)
    , graph, evalGraph
    , flipGraph
    , mapEdges
    )
where
import Prelude hiding (head, tail)
import Sharing()
import Downhill.Linear.Graph.NodeMap
    ( NodeSet, NodeMap, NodeKey, SomeItem(SomeItem), List2(List2) )
import Data.Either (partitionEithers)
import qualified Downhill.Linear.Graph.NodeMap as NodeMap
import Downhill.Linear.Graph.Types (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder), FwdFun (unFwdFun))

data Graph s e da dz = BasicVector da => Graph (NodeMap s (Node (NodeKey s) e da)) (Node (NodeKey s) e da dz)

data SomeGraph e a z where
  SomeGraph :: NodeSet s => Graph s e a z -> SomeGraph e a z

data AnyEdge s e da dz = forall du dv. AnyEdge (Endpoint (NodeKey s) dz dv) (e du dv) (Endpoint (NodeKey s) da du)

evalGraph :: forall s dx dz. Graph s FwdFun dz dx -> dz -> dx
evalGraph (Graph nodes finalNode) dz = evalNode finalNode
    where
          evalParent :: forall dv. Endpoint (NodeKey s) dz dv -> dv
          evalParent tail = goTail tail
              where goTail :: forall dx'. Endpoint (NodeKey s) dz dx' -> dx'
                    goTail = \case
                      SourceNode -> dz
                      InnerNode nodeName -> runIdentity (NodeMap.lookup innerValues nodeName)
          evalEdge :: Edge (NodeKey s) FwdFun dz dv -> VecBuilder dv
          evalEdge (Edge f tail) = unFwdFun f $ evalParent tail
          evalNode :: Node (NodeKey s) FwdFun dz dv -> dv
          evalNode (Node xs) = sumBuilder (mconcat [evalEdge x | x <- xs])
          evalGraphInnerNodes :: NodeMap s (Node (NodeKey s) FwdFun dz) -> NodeMap s Identity
          evalGraphInnerNodes = NodeMap.mapmap (Identity . evalNode)
          innerValues :: NodeMap s Identity
          innerValues = evalGraphInnerNodes nodes

nodeEdges :: forall s f da dz dx. NodeKey s dx -> Node (NodeKey s) f da dx -> [AnyEdge s f da dz]
nodeEdges name (Node xs) = go <$> xs
    where go :: Edge (NodeKey s) f da dx -> AnyEdge s f da dz
          go (Edge f head) = AnyEdge (InnerNode name) f head

allGraphEdges :: forall s f da dz. Graph s f da dz -> [AnyEdge s f da dz]
allGraphEdges (Graph env (Node es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s f da dz]
          innerEdges = concatMap nodeEdges' (NodeMap.toList env)
            where nodeEdges' (NodeMap.SomeItem name node) = nodeEdges name node
          finalEdges :: [AnyEdge s f da dz]
          finalEdges = go <$> es
            where go :: Edge (NodeKey s) f da dz -> AnyEdge s f da dz
                  go (Edge f head) = AnyEdge SourceNode f head

classifyTail
  :: forall s f da dz.
     AnyEdge s f da dz
  -> Either (Edge (NodeKey s) f da dz) (SomeItem s (Edge (NodeKey s) f da))
classifyTail (AnyEdge tail f head) = case tail of
    SourceNode  -> Left (Edge f head)
    InnerNode x -> Right (SomeItem x (Edge f head))

flipAnyEdge :: (forall u v. f u v -> g v u) -> AnyEdge s f da dz -> AnyEdge s g dz da
flipAnyEdge flipF (AnyEdge tail f head) = AnyEdge head (flipF f) tail

data NodeDict dx = BasicVector dx => NodeDict

edgeListToGraph
  :: forall s f da dz. (NodeSet s, BasicVector da, BasicVector dz)
  => NodeMap s NodeDict
  -> [AnyEdge s f dz da]
  -> Graph s f dz da
edgeListToGraph dictmap flippedEdges = Graph edgeMap (Node initial)
    where initial :: [Edge (NodeKey s) f dz da]
          inner :: [SomeItem s (Edge (NodeKey s) f dz)]
          (initial, inner) = partitionEithers (classifyTail <$> flippedEdges)
          edgeList :: NodeMap s (List2 (Edge (NodeKey s) f dz))
          edgeList = NodeMap.fromList inner
          edgeMap :: NodeMap s (Node (NodeKey s) f dz)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (Edge (NodeKey s) f dz) dx -> Node (NodeKey s) f dz dx
          withDict NodeDict (List2 xs) = Node xs

backFromEdges
  :: forall s f g da dz. (NodeSet s, BasicVector da, BasicVector dz)
  => (forall u v. f u v -> g v u)
  -> NodeMap s NodeDict
  -> [AnyEdge s f da dz]
  -> Graph s g dz da
backFromEdges flipFunc dictmap edges = edgeListToGraph dictmap flippedEdges
  where flippedEdges :: [AnyEdge s g dz da]
        flippedEdges = flipAnyEdge flipFunc <$> edges

graphNodes :: Graph s f da dz -> NodeMap s NodeDict
graphNodes (Graph env _) = NodeMap.mapmap go env
    where go :: Node (NodeKey s) f da dv -> NodeDict dv
          go = \case
            Node _ -> NodeDict

flipGraph :: NodeSet s => (forall u v. f u v -> g v u) -> Graph s f da dz -> Graph s g dz da
flipGraph flipEdge g@(Graph _ (Node _)) = backFromEdges flipEdge (graphNodes g) (allGraphEdges g)

mapEdges :: forall s f g da dz. (forall u v. f u v -> g u v) -> Graph s f da dz -> Graph s g da dz
mapEdges f (Graph inner final) = Graph (NodeMap.mapmap go inner) (go final)
  where go :: Node (NodeKey s) f da dv -> Node (NodeKey s) g da dv
        go (Node xs) = Node [goEdge x | x <- xs]
        goEdge :: Edge p f da dx -> Edge p g da dx
        goEdge (Edge e x) = Edge (f e) x

graph :: BasicVector a => NodeMap s (Node (NodeKey s) e a) -> Node (NodeKey s) e a z -> Graph s e a z
graph = Graph
