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
    ( ForwardGraph(..)
    , BackwardGraph
    , Head(..)
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

data Head s da dz dx where
    SourceHead :: Head s da dz da
    InnerHead :: NodeKey s dx -> Head s da dz dx

data ForwardEdge s f da dz dv where
    ForwardEdge :: f du dv -> Head s da dz du -> ForwardEdge s f da dz dv

type ForwardNode s f da dz = VectorSum (ForwardEdge s f da dz)
--type BackwardNode s f da dz = VectorSum (ForwardEdge s f dz da)

data ForwardGraph s f da dz = ForwardGraph (NodeMap s (ForwardNode s f da dz)) (ForwardNode s f da dz dz)
type BackwardGraph s da dz = ForwardGraph s BackFunction1 dz da

data AnyEdge s f da dz = forall du dv. AnyEdge (Head s dz da dv) (f du dv) (Head s da dz du)

convertGraph :: forall s da dz. NodeMap s (SharedExprS s da) -> SharedExprS s da dz -> ForwardGraph s AFunction1 da dz
convertGraph env (VectorSum zs) = ForwardGraph (NodeMap.mapmap convertInnerNode env) (VectorSum (convertFinalEdge <$> zs))
    where convertInnerNode :: forall dx. SharedExprS s da dx -> ForwardNode s AFunction1 da dz dx
          convertInnerNode (VectorSum xs) = VectorSum (convertInnerEdge <$> xs)
          convertInnerEdge :: forall dx. SharedTermS s da dx -> ForwardEdge s AFunction1 da dz dx
          convertInnerEdge = \case
            Func2 f x -> ForwardEdge f (convertArg x)
          convertArg :: SharedArgS s da du -> Head s da dz du
          convertArg = \case
             ArgVar -> SourceHead
             ArgExpr argName -> InnerHead argName
          convertFinalEdge :: SharedTermS s da dz -> ForwardEdge s AFunction1 da dz dz
          convertFinalEdge = \case
            Func2 f x -> ForwardEdge f (convertArg x)

newtype BackValue dx = BackValue dx
    deriving Generic

goBackEdge' :: forall s da dz du dv. BasicVector du => NodeMap s BackValue -> dz -> Head s dz da dv -> BackFunction1 dv du -> VecBuilder du
goBackEdge' ys dz tail f = backF1' f (goTail tail)
    where goTail :: Head s dz da dx -> dx
          goTail = \case
            SourceHead -> dz
            InnerHead nodeName -> case NodeMap.lookup ys nodeName of
                BackValue x -> x

evalBackMap :: forall s da dz. NodeMap s (ForwardNode s BackFunction1 dz da) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. ForwardNode s BackFunction1 dz da dx -> BackValue dx
          go (VectorSum xs') = BackValue (sumBuilder (goEdge <$> xs'))
          goEdge :: BasicVector dx => ForwardEdge s BackFunction1 dz da dx -> VecBuilder dx
          goEdge = \case
            ForwardEdge f tail -> goBackEdge' ys dz tail f

instance BasicVector da => TensorProduct dz (ForwardGraph s BackFunction1 dz da) da where
    dx ⊗ ForwardGraph env (VectorSum edges) = sumBuilder (go <$> edges)
        where go :: ForwardEdge s BackFunction1 dz da da -> VecBuilder da
              go (ForwardEdge f tail) = goBackEdge' env' dx tail f
              env' = evalBackMap env dx

forwardNodeEdges :: forall s f da dz dx. NodeKey s dx -> ForwardNode s f da dz dx -> [AnyEdge s f da dz]
forwardNodeEdges name (VectorSum xs) = go <$> xs
    where go :: ForwardEdge s f da dz dx -> AnyEdge s f da dz
          go (ForwardEdge f head) = AnyEdge (InnerHead name) f head

allFwdEdges :: forall s f da dz. ForwardGraph s f da dz -> [AnyEdge s f da dz]
allFwdEdges (ForwardGraph env (VectorSum es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s f da dz]
          innerEdges = concatMap nodeEdges (NodeMap.toList env)
            where nodeEdges (NodeMap.SomeItem name node) = forwardNodeEdges name node
          finalEdges :: [AnyEdge s f da dz]
          finalEdges = go <$> es
            where go :: ForwardEdge s f da dz dz -> AnyEdge s f da dz
                  go (ForwardEdge f head) = AnyEdge SourceHead f head

classifyTail
  :: forall s f da dz.
     AnyEdge s f da dz
  -> Either (ForwardEdge s f da dz dz) (SomeItem s (ForwardEdge s f da dz))
classifyTail (AnyEdge tail f head) = case tail of
    SourceHead -> Left (ForwardEdge f head) -- Left (ForwardEdge (flipFunc f) tail)
    InnerHead x -> Right (SomeItem x (ForwardEdge f head))

flipAnyEdge :: (forall u v. f u v -> g v u) -> AnyEdge s f da dz -> AnyEdge s g dz da
flipAnyEdge flipF (AnyEdge tail f head) = AnyEdge head (flipF f) tail

data NodeDict dx = BasicVector dx => NodeDict

edgeListToGraph
  :: forall s f da dz. (NodeSet s, BasicVector da)
  => NodeMap s NodeDict
  -> [AnyEdge s f dz da]
  -> ForwardGraph s f dz da
edgeListToGraph dictmap flippedEdges = ForwardGraph edgeMap (VectorSum initial)
    where initial :: [ForwardEdge s f dz da da]
          inner :: [SomeItem s (ForwardEdge s f dz da)]
          (initial, inner) = partitionEithers (classifyTail <$> flippedEdges)
          edgeList :: NodeMap s (List2 (ForwardEdge s f dz da))
          edgeList = NodeMap.fromList inner
          edgeMap :: NodeMap s (ForwardNode s f dz da)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (ForwardEdge s f dz da) dx -> ForwardNode s f dz da dx
          withDict NodeDict (List2 xs) = VectorSum xs

backFromEdges
  :: forall s f g da dz. (NodeSet s, BasicVector da)
  => (forall u v. f u v -> g v u)
  -> NodeMap s NodeDict
  -> [AnyEdge s f da dz]
  -> ForwardGraph s g dz da
backFromEdges flipFunc dictmap edges = edgeListToGraph dictmap flippedEdges
  where
          flippedEdges :: [AnyEdge s g dz da]
          flippedEdges = flipAnyEdge flipFunc <$> edges

mkdict :: ForwardGraph s AFunction1 da dz -> NodeMap s NodeDict
mkdict (ForwardGraph env _) = NodeMap.mapmap go env
    where go :: ForwardNode s AFunction1 da dz dv -> NodeDict dv
          go = \case
            VectorSum _ -> NodeDict

flipGraph :: (NodeSet s, BasicVector da) => ForwardGraph s AFunction1 da dz -> BackwardGraph s da dz
flipGraph fwd = backFromEdges flipFunc1 (mkdict fwd) (allFwdEdges fwd)
