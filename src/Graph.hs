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
    , Endpoint(..)
    , convertGraph
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
import Expr (ExprArg(ArgExpr, ArgVar), Term3(Func2))
import Notensor
    ( BasicVectors, BasicVector(VecBuilder, sumBuilder)
    , AFunction2(backF, fwdF, AFunction2), AFunction1(AFunction1)
    , backF1, BackFunction1 (backF1'), flipFunc1
    )
import EType (VectorSum(VectorSum))

-- IDEA: `TensorProduct (AFunction1 du dv) (BackValue du)` instance instead
-- of `TensorProduct (AFunction1 du dv) du`, avoiding overlap issues
newtype BackValue dx = BackValue dx

data Endpoint s da dz dx where
    SourceNode :: Endpoint s da dz da
    InnerNode :: NodeKey s dx -> Endpoint s da dz dx

data Edge s e da dz dv where
    Edge :: e du dv -> Endpoint s da dz du -> Edge s e da dz dv

type Node s e da dz = VectorSum (Edge s e da dz)

data Graph s e da dz = Graph (NodeMap s (Node s e da dz)) (Node s e da dz dz)

data AnyEdge s f da dz = forall du dv. AnyEdge (Endpoint s dz da dv) (f du dv) (Endpoint s da dz du)

convertGraph :: forall s da dz. NodeMap s (SharedExprS s da) -> SharedExprS s da dz -> Graph s AFunction1 da dz
convertGraph env (VectorSum zs) = Graph (NodeMap.mapmap convertInnerNode env) (VectorSum (convertFinalEdge <$> zs))
    where convertInnerNode :: forall dx. SharedExprS s da dx -> Node s AFunction1 da dz dx
          convertInnerNode (VectorSum xs) = VectorSum (convertInnerEdge <$> xs)
          convertInnerEdge :: forall dx. SharedTermS s da dx -> Edge s AFunction1 da dz dx
          convertInnerEdge = \case
            Func2 f x -> Edge f (convertArg x)
          convertArg :: SharedArgS s da du -> Endpoint s da dz du
          convertArg = \case
             ArgVar -> SourceNode
             ArgExpr argName -> InnerNode argName
          convertFinalEdge :: SharedTermS s da dz -> Edge s AFunction1 da dz dz
          convertFinalEdge = \case
            Func2 f x -> Edge f (convertArg x)

lookupParentValue :: forall s da dz dv. NodeMap s BackValue -> dz -> Endpoint s dz da dv -> dv
lookupParentValue ys dz tail = (goTail tail)
    where goTail :: Endpoint s dz da dx -> dx
          goTail = \case
            SourceNode -> dz
            InnerNode nodeName -> case NodeMap.lookup ys nodeName of
                BackValue x -> x

goBackEdge' :: forall s da dz du dv. BasicVector du => NodeMap s BackValue -> dz -> Endpoint s dz da dv -> BackFunction1 dv du -> VecBuilder du
goBackEdge' ys dz tail f = backF1' f (lookupParentValue ys dz tail)

evalBackMap :: forall s da dz. NodeMap s (Node s BackFunction1 dz da) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. Node s BackFunction1 dz da dx -> BackValue dx
          go (VectorSum xs') = BackValue (sumBuilder (goEdge <$> xs'))
          goEdge :: BasicVector dx => Edge s BackFunction1 dz da dx -> VecBuilder dx
          goEdge = \case
            Edge f tail -> goBackEdge' ys dz tail f

instance BasicVector da => TensorProduct dz (Graph s BackFunction1 dz da) da where
    dx ⊗ Graph env (VectorSum edges) = sumBuilder (go <$> edges)
        where go :: Edge s BackFunction1 dz da da -> VecBuilder da
              go (Edge f tail) = goBackEdge' env' dx tail f
              env' = evalBackMap env dx

forwardNodeEdges :: forall s f da dz dx. NodeKey s dx -> Node s f da dz dx -> [AnyEdge s f da dz]
forwardNodeEdges name (VectorSum xs) = go <$> xs
    where go :: Edge s f da dz dx -> AnyEdge s f da dz
          go (Edge f head) = AnyEdge (InnerNode name) f head

allFwdEdges :: forall s f da dz. Graph s f da dz -> [AnyEdge s f da dz]
allFwdEdges (Graph env (VectorSum es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s f da dz]
          innerEdges = concatMap nodeEdges (NodeMap.toList env)
            where nodeEdges (NodeMap.SomeItem name node) = forwardNodeEdges name node
          finalEdges :: [AnyEdge s f da dz]
          finalEdges = go <$> es
            where go :: Edge s f da dz dz -> AnyEdge s f da dz
                  go (Edge f head) = AnyEdge SourceNode f head

classifyTail
  :: forall s f da dz.
     AnyEdge s f da dz
  -> Either (Edge s f da dz dz) (SomeItem s (Edge s f da dz))
classifyTail (AnyEdge tail f head) = case tail of
    SourceNode -> Left (Edge f head)
    InnerNode x -> Right (SomeItem x (Edge f head))

flipAnyEdge :: (forall u v. f u v -> g v u) -> AnyEdge s f da dz -> AnyEdge s g dz da
flipAnyEdge flipF (AnyEdge tail f head) = AnyEdge head (flipF f) tail

data NodeDict dx = BasicVector dx => NodeDict

edgeListToGraph
  :: forall s f da dz. (NodeSet s, BasicVector da)
  => NodeMap s NodeDict
  -> [AnyEdge s f dz da]
  -> Graph s f dz da
edgeListToGraph dictmap flippedEdges = Graph edgeMap (VectorSum initial)
    where initial :: [Edge s f dz da da]
          inner :: [SomeItem s (Edge s f dz da)]
          (initial, inner) = partitionEithers (classifyTail <$> flippedEdges)
          edgeList :: NodeMap s (List2 (Edge s f dz da))
          edgeList = NodeMap.fromList inner
          edgeMap :: NodeMap s (Node s f dz da)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (Edge s f dz da) dx -> Node s f dz da dx
          withDict NodeDict (List2 xs) = VectorSum xs

backFromEdges
  :: forall s f g da dz. (NodeSet s, BasicVector da)
  => (forall u v. f u v -> g v u)
  -> NodeMap s NodeDict
  -> [AnyEdge s f da dz]
  -> Graph s g dz da
backFromEdges flipFunc dictmap edges = edgeListToGraph dictmap flippedEdges
  where
          flippedEdges :: [AnyEdge s g dz da]
          flippedEdges = flipAnyEdge flipFunc <$> edges

mkdict :: Graph s AFunction1 da dz -> NodeMap s NodeDict
mkdict (Graph env _) = NodeMap.mapmap go env
    where go :: Node s AFunction1 da dz dv -> NodeDict dv
          go = \case
            VectorSum _ -> NodeDict

flipGraph :: (NodeSet s, BasicVector da) => Graph s AFunction1 da dz -> Graph s BackFunction1 dz da
flipGraph fwd = backFromEdges flipFunc1 (mkdict fwd) (allFwdEdges fwd)
