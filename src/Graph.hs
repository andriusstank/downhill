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
    , ForwardEdge(..)
    , BackwardGraph(..)
    , BackwardNode
    , BackwardEdge(..)
    , Head(..), Tail(..)
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
    , backF1
    )
import EType (VectorSum(VectorSum))

data Head s da dz dx where
    SourceHead :: Head s da dz da
    InnerHead :: NodeKey s dx -> Head s da dz dx

data ForwardEdge s da dz dv where
    ForwardEdge :: AFunction1 du dv -> Head s da dz du -> ForwardEdge s da dz dv

type ForwardNode s da dz = VectorSum (ForwardEdge s da dz)

data ForwardGraph s da dz = ForwardGraph (NodeMap s (ForwardNode s da dz)) (ForwardNode s da dz dz)


data Tail s da dz dx where
    SinkTail :: Tail s da dz dz
    InnerTail :: NodeKey s dx -> Tail s da dz dx

data BackwardEdge s da dz du where
    BackwardEdge :: Tail s da dz dv -> AFunction1 du dv -> BackwardEdge s da dz du

type BackwardNode s da dz = VectorSum (BackwardEdge s da dz)

data BackwardGraph s da dz = BackwardGraph (NodeMap s (BackwardNode s da dz)) (BackwardNode s da dz da)

data AnyEdge s da dz = forall du dv. AnyEdge (Tail s da dz dv) (AFunction1 du dv) (Head s da dz du)

convertGraph :: forall s da dz. NodeMap s (SharedExprS s da) -> SharedExprS s da dz -> ForwardGraph s da dz
convertGraph env (VectorSum zs) = ForwardGraph (NodeMap.mapmap convertInnerNode env) (VectorSum (convertFinalEdge <$> zs))
    where convertInnerNode :: forall dx. SharedExprS s da dx -> ForwardNode s da dz dx
          convertInnerNode (VectorSum xs) = VectorSum (convertInnerEdge <$> xs)
          convertInnerEdge :: forall dx. SharedTermS s da dx -> ForwardEdge s da dz dx
          convertInnerEdge = \case
            Func2 f x -> ForwardEdge f (convertArg x)
          convertArg :: SharedArgS s da du -> Head s da dz du
          convertArg = \case
             ArgVar -> SourceHead
             ArgExpr argName -> InnerHead argName
          convertFinalEdge :: SharedTermS s da dz -> ForwardEdge s da dz dz
          convertFinalEdge = \case
            Func2 f x -> ForwardEdge f (convertArg x)

newtype BackValue dx = BackValue dx
    deriving Generic

goBackEdge' :: forall s da dz du dv. BasicVector du => NodeMap s BackValue -> dz -> Tail s da dz dv -> AFunction1 du dv -> VecBuilder du
goBackEdge' ys dz tail f = backF1 f (goTail tail)
    where goTail :: Tail s da dz dx -> dx
          goTail = \case
            SinkTail -> dz
            InnerTail nodeName -> case NodeMap.lookup ys nodeName of
                BackValue x -> x

evalBackMap :: forall s da dz. NodeMap s (BackwardNode s da dz) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. BackwardNode s da dz dx -> BackValue dx
          go (VectorSum xs') = BackValue (sumBuilder (goEdge <$> xs'))
          goEdge :: BasicVector dx => BackwardEdge s da dz dx -> VecBuilder dx
          goEdge = \case
            BackwardEdge tail f -> goBackEdge' ys dz tail f

instance BasicVector da => TensorProduct dz (BackwardGraph s da dz) da where
    dx ⊗ BackwardGraph env (VectorSum edges) = sumBuilder (go <$> edges)
        where go :: BackwardEdge s da dz da -> VecBuilder da
              go (BackwardEdge tail f) = goBackEdge' env' dx tail f
              env' = evalBackMap env dx

forwardNodeEdges :: forall s da dz dx. NodeKey s dx -> ForwardNode s da dz dx -> [AnyEdge s da dz]
forwardNodeEdges name (VectorSum xs) = go <$> xs
    where go :: ForwardEdge s da dz dx -> AnyEdge s da dz
          go (ForwardEdge f head) = AnyEdge (InnerTail name) f head

allFwdEdges :: forall s da dz. ForwardGraph s da dz -> [AnyEdge s da dz]
allFwdEdges (ForwardGraph env (VectorSum es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s da dz]
          innerEdges = concatMap nodeEdges (NodeMap.toList env)
            where nodeEdges (NodeMap.SomeItem name node) = forwardNodeEdges name node
          finalEdges :: [AnyEdge s da dz]
          finalEdges = go <$> es
            where go :: ForwardEdge s da dz dz -> AnyEdge s da dz
                  go (ForwardEdge f head) = AnyEdge SinkTail f head

classifyBackEdge
  :: forall s da dz.
     AnyEdge s da dz
  -> Either (BackwardEdge s da dz da) (SomeItem s (BackwardEdge s da dz))
classifyBackEdge (AnyEdge tail f head) = case head of
    SourceHead -> Left (BackwardEdge tail f)
    InnerHead x -> Right (SomeItem x (BackwardEdge tail f))

data NodeDict dx = BasicVector dx => NodeDict

cvItemS :: SomeItem s f -> SomeItem s f
cvItemS (SomeItem x y) = SomeItem x y

backFromEdges :: forall s da dz. (NodeSet s, BasicVector da) => NodeMap s NodeDict -> [AnyEdge s da dz] -> BackwardGraph s da dz
backFromEdges dictmap edges = BackwardGraph edgeMap (VectorSum initial)
    where (initial, inner) = partitionEithers (classifyBackEdge <$> edges)
          edgeList :: NodeMap s (List2 (BackwardEdge s da dz))
          edgeList = NodeMap.fromList [NodeMap.SomeItem xname (mkNode xname x) | (NodeMap.SomeItem xname x) <- (cvItemS <$> inner)]
            where mkNode :: NodeKey s dv -> BackwardEdge s da dz dv -> BackwardEdge s da dz dv
                  mkNode xname x = case (NodeMap.lookup dictmap xname) of
                      NodeDict -> x
          edgeMap :: NodeMap s (BackwardNode s da dz)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (BackwardEdge s da dz) dx -> BackwardNode s da dz dx
          withDict NodeDict (List2 xs) = VectorSum xs

mkdict :: ForwardGraph s da dz -> NodeMap s NodeDict
mkdict (ForwardGraph env _) = NodeMap.mapmap go env
    where go :: ForwardNode s da dz dv -> NodeDict dv
          go = \case
            VectorSum _ -> NodeDict

flipGraph :: (NodeSet s, BasicVector da) => ForwardGraph s da dz -> BackwardGraph s da dz
flipGraph fwd = backFromEdges (mkdict fwd) (allFwdEdges fwd)
