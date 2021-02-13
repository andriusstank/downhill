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
{-# OPTIONS_GHC -Wno-unused-imports #-}
 
module Graph
    ( Graph(..)
    , flipGraph
    )
where
import Prelude hiding (head, tail)
import Sharing()
import Tensor(transposeFunc, LinearFunction, TensorProduct(..))

import NodeMap (NodeSet, SharedArgS, SharedTermS,  SharedExprS,  NodeMap, NodeKey, SomeItem(SomeItem), List2(List2))
import Data.VectorSpace (sumV, AdditiveGroup)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Either (partitionEithers)
import NodeMap ()

import qualified NodeMap
import Notensor
    ( BasicVectors, BasicVector(VecBuilder, sumBuilder)
    , AFunction2(backF, fwdF, AFunction2), AFunction1(AFunction1)
    , backF1, BackFunction1 (backF1'), flipFunc1
    )
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(..))

-- IDEA: `TensorProduct (AFunction1 du dv) (BackValue du)` instance instead
-- of `TensorProduct (AFunction1 du dv) du`, avoiding overlap issues
newtype BackValue dx = BackValue { unBackValue :: dx }

data Graph s e da dz = Graph (NodeMap s (Node (NodeKey s) e da)) (Node (NodeKey s) e da dz)

data AnyEdge s f da dz = forall du dv. AnyEdge (Endpoint (NodeKey s) dz dv) (f du dv) (Endpoint (NodeKey s) da du)

data NodeValues s da = NodeValues (NodeMap s BackValue) da

lookupParentValue :: forall s dz dv. NodeValues s dz -> Endpoint (NodeKey s) dz dv -> dv
lookupParentValue (NodeValues ys dz) tail = (goTail tail)
    where goTail :: Endpoint (NodeKey s) dz dx -> dx
          goTail = \case
            SourceNode -> dz
            InnerNode nodeName -> case NodeMap.lookup ys nodeName of
                BackValue x -> x

evalBackEdge' :: forall s dz du dv. BasicVector du => NodeValues s dz -> Endpoint (NodeKey s) dz dv -> BackFunction1 dv du -> VecBuilder du
evalBackEdge' nodes tail f = backF1' f (lookupParentValue nodes tail)

evalNode :: forall s dz dx. NodeValues s dz -> Node (NodeKey s) BackFunction1 dz dx -> BackValue dx
evalNode nodes (Node xs') = BackValue (sumBuilder (goEdge <$> xs'))
  where goEdge :: BasicVector dx => Edge (NodeKey s) BackFunction1 dz dx -> VecBuilder dx
        goEdge (Edge f tail) = evalBackEdge' nodes tail f

evalBackMap :: forall s dz. dz -> NodeMap s (Node (NodeKey s) BackFunction1 dz) -> NodeMap s BackValue
evalBackMap dz dxs = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. Node (NodeKey s) BackFunction1 dz dx -> BackValue dx
          go = evalNode (NodeValues ys dz)

evalBackMap' :: forall s dz. dz -> NodeMap s (Node (NodeKey s) BackFunction1 dz) -> NodeValues s dz
evalBackMap' dz dxs = NodeValues (evalBackMap dz dxs) dz

instance BasicVector da => TensorProduct dz (Graph s BackFunction1 dz da) da where
    dx ⊗ Graph env node = unBackValue (evalNode env' node)
        where env' = evalBackMap' dx env

nodeEdges :: forall s f da dz dx. NodeKey s dx -> Node (NodeKey s) f da dx -> [AnyEdge s f da dz]
nodeEdges name (Node xs) = go <$> xs
    where go :: Edge (NodeKey s) f da dx -> AnyEdge s f da dz
          go (Edge f head) = AnyEdge (InnerNode name) f head

allFwdEdges :: forall s f da dz. Graph s f da dz -> [AnyEdge s f da dz]
allFwdEdges (Graph env (Node es)) = finalEdges ++ innerEdges
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
  :: forall s f da dz. (NodeSet s, BasicVector da)
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
  :: forall s f g da dz. (NodeSet s, BasicVector da)
  => (forall u v. f u v -> g v u)
  -> NodeMap s NodeDict
  -> [AnyEdge s f da dz]
  -> Graph s g dz da
backFromEdges flipFunc dictmap edges = edgeListToGraph dictmap flippedEdges
  where flippedEdges :: [AnyEdge s g dz da]
        flippedEdges = flipAnyEdge flipFunc <$> edges

mkdict :: Graph s AFunction1 da dz -> NodeMap s NodeDict
mkdict (Graph env _) = NodeMap.mapmap go env
    where go :: Node (NodeKey s) AFunction1 da dv -> NodeDict dv
          go = \case
            Node _ -> NodeDict

flipGraph :: (NodeSet s, BasicVector da) => Graph s AFunction1 da dz -> Graph s BackFunction1 dz da
flipGraph fwd = backFromEdges flipFunc1 (mkdict fwd) (allFwdEdges fwd)
