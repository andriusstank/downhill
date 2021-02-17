{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
 
module Graph
    ( Graph(..)
    , flipGraph
    , mapEdges
    )
where
import Prelude hiding (head, tail)
import Sharing()
import Tensor(TensorProduct(..), Vec (Vec))

import NodeMap (NodeSet,  NodeMap, NodeKey, SomeItem(SomeItem), List2(List2))
import Data.Either (partitionEithers)
import NodeMap ()

import qualified NodeMap
import Notensor(FwdFunc, BasicVector (VecBuilder, sumBuilder), Transpose(..))
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))
import Data.Constraint (Dict(Dict))

data Graph s e da dz = BasicVector da => Graph (NodeMap s (Node (NodeKey s) e da)) (Node (NodeKey s) e da dz)

data AnyEdge s e da dz = forall du dv. AnyEdge (Endpoint (NodeKey s) dz dv) (e du dv) (Endpoint (NodeKey s) da du)

data NodeValues s da = NodeValues (NodeMap s Vec) (Vec da)

instance BasicVector da => TensorProduct (Graph s FwdFunc dz da) (Vec dz) where
    type (Graph s FwdFunc dz da) ⊗ (Vec dz) = Vec da
    g ⊗ dx = evalGraph g dx
  
lookupParent :: forall s dz dv. NodeValues s dz -> Endpoint (NodeKey s) dz dv -> Vec dv
lookupParent (NodeValues ys dz) tail = (goTail tail)
    where goTail :: Endpoint (NodeKey s) dz dx -> Vec dx
          goTail = \case
            SourceNode -> dz
            InnerNode nodeName -> NodeMap.lookup ys nodeName

evalGraph :: forall s dx dz. Graph s FwdFunc dz dx -> Vec dz -> Vec dx
evalGraph (Graph nodes finalNode) dz = evalNode finalNode
    where
          allValues :: NodeValues s dz
          allValues = NodeValues innerValues dz
          evalParent :: Endpoint (NodeKey s) dz dv -> Vec dv
          evalParent = lookupParent allValues
          evalEdge :: (Edge (NodeKey s) FwdFunc dz dv -> VecBuilder dv)
          evalEdge (Edge f tail) = f ⊗ evalParent tail
          evalNode :: (Node (NodeKey s) FwdFunc dz dv -> Vec dv)
          evalNode (Node xs) = Vec (sumBuilder [evalEdge x | x <- xs])
          evalGraphInnerNodes :: (NodeMap s (Node (NodeKey s) FwdFunc dz) -> NodeMap s Vec)
          evalGraphInnerNodes = NodeMap.mapmap evalNode
          innerValues :: NodeMap s Vec
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

instance (NodeSet s, Transpose f g) => Transpose (Graph s f) (Graph s g) where
  transpose = flipGraph
  flipTranspose = case flipTranspose @f @g of
    Dict -> Dict

flipGraph :: (NodeSet s, Transpose f g) => Graph s f da dz -> Graph s g dz da
flipGraph g@(Graph _ (Node _)) = backFromEdges transpose (graphNodes g) (allGraphEdges g)

mapEdges :: forall s f g da dz. (forall u v. f u v -> g u v) -> Graph s f da dz -> Graph s g da dz
mapEdges f (Graph inner final) = Graph (NodeMap.mapmap go inner) (go final)
  where go :: Node (NodeKey s) f da dv -> Node (NodeKey s) g da dv
        go (Node xs) = Node [goEdge x | x <- xs]
        goEdge :: Edge p f da dx -> Edge p g da dx
        goEdge (Edge e x) = Edge (f e) x