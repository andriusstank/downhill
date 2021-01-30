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
    , ForwardInnerNode(..), ForwardFinalNode(..)
    , SomeForwardInnerEdge(..), SomeForwardFinalEdge
    , BackwardGraph(..)
    , BackwardInnerNode(..), BackwardInitialNode(..)
    , SomeBackwardInnerEdge(..),SomeBackwardInitialEdge(..)
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
import Expr (ExprArg(ArgExpr, ArgVar), Term3(Func2), Expr3(ExprSum))
import Notensor
    ( BasicVectors, BasicVector(VecBuilder, sumBuilder)
    , AFunction2(backF, fwdF, AFunction2), AFunction1(AFunction1)
    , backF1
    )

data Head s da dz dx where
    SourceHead :: Head s da dz da
    InnerHead :: NodeKey s dx -> Head s da dz dx

data Tail s da dz dx where
    SinkTail :: Tail s da dz dz
    InnerTail :: NodeKey s dx -> Tail s da dz dx

data ForwardEdge s da dz du dv = ForwardEdge (AFunction1 du dv) (Head s da dz du)
data BackwardEdge s da dz du dv = BackwardEdge (Tail s da dz dv) (AFunction1 du dv)

data SomeForwardInnerEdge s da dz dv = forall du. SomeForwardInnerEdge (ForwardEdge s da dz du dv)
data SomeForwardFinalEdge s da dz = forall du. SomeForwardFinalEdge (ForwardEdge s da dz du dz)

data ForwardInnerNode s da dz dx where
    ForwardInnerNode :: BasicVector dx => [SomeForwardInnerEdge s da dz dx] -> ForwardInnerNode s da dz dx

data ForwardFinalNode s da dz = ForwardFinalNode [SomeForwardFinalEdge s da dz]

data ForwardGraph s da dz = ForwardGraph (NodeMap s (ForwardInnerNode s da dz)) (ForwardFinalNode s da dz)

data SomeBackwardInnerEdge s da dz du = forall dv. SomeBackwardInnerEdge (BackwardEdge s da dz du dv)
data SomeBackwardInitialEdge s da dz = forall dv. SomeBackwardInitialEdge (BackwardEdge s da dz da dv)

data BackwardInnerNode s da dz dx where
    BackwardInnerNode :: BasicVector dx => [SomeBackwardInnerEdge s da dz dx] -> BackwardInnerNode s da dz dx

data BackwardInitialNode s da dz = BackwardInitialNode [SomeBackwardInitialEdge s da dz]

data BackwardGraph s da dz = BackwardGraph (NodeMap s (BackwardInnerNode s da dz)) (BackwardInitialNode s da dz)

data AnyEdge s da dz = forall du dv. AnyEdge (Tail s da dz dv) (AFunction1 du dv) (Head s da dz du)

convertGraph :: forall s da dz. NodeMap s (SharedExprS s da) -> SharedExprS s da dz -> ForwardGraph s da dz
convertGraph env (ExprSum zs) = ForwardGraph (NodeMap.mapmap convertInnerNode env) (ForwardFinalNode (convertFinalEdge <$> zs))
    where convertInnerNode :: forall dx. SharedExprS s da dx -> ForwardInnerNode s da dz dx
          convertInnerNode (ExprSum xs) = ForwardInnerNode (convertInnerEdge <$> xs)
          convertInnerEdge :: forall dx. SharedTermS s da dx -> SomeForwardInnerEdge s da dz dx
          convertInnerEdge = \case
            Func2 f x -> SomeForwardInnerEdge (ForwardEdge f (convertArg x))
          convertArg :: SharedArgS s da du -> Head s da dz du
          convertArg = \case
             ArgVar -> SourceHead
             ArgExpr argName -> InnerHead argName
          convertFinalEdge :: SharedTermS s da dz -> SomeForwardFinalEdge s da dz
          convertFinalEdge = \case
            Func2 f x -> SomeForwardFinalEdge (ForwardEdge f (convertArg x))

newtype BackValue dx = BackValue dx
    deriving Generic

goBackEdge' :: forall s da dz du dv. BasicVector du => NodeMap s BackValue -> dz -> BackwardEdge s da dz du dv -> VecBuilder du
goBackEdge' ys dz (BackwardEdge tail f) = backF1 f (goTail tail)
    where goTail :: Tail s da dz dx -> dx
          goTail = \case
            SinkTail -> dz
            InnerTail nodeName -> case NodeMap.lookup ys nodeName of
                BackValue x -> x

evalBackMap :: forall s da dz. NodeMap s (BackwardInnerNode s da dz) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. BackwardInnerNode s da dz dx -> BackValue dx
          go (BackwardInnerNode xs') = BackValue (sumBuilder (goEdge <$> xs'))
          goEdge :: BasicVector dx => SomeBackwardInnerEdge s da dz dx -> VecBuilder dx
          goEdge = \case
            SomeBackwardInnerEdge e -> goBackEdge' ys dz e

instance BasicVector da => TensorProduct dz (BackwardGraph s da dz) da where
    dx ⊗ BackwardGraph env (BackwardInitialNode edges) = sumBuilder (go <$> edges)
        where go :: SomeBackwardInitialEdge s da dz -> VecBuilder da
              go (SomeBackwardInitialEdge edge) = goBackEdge' env' dx edge
              env' = evalBackMap env dx

forwardNodeEdges :: forall s da dz dx. NodeKey s dx -> ForwardInnerNode s da dz dx -> [AnyEdge s da dz]
forwardNodeEdges name (ForwardInnerNode xs) = go <$> xs
    where go :: SomeForwardInnerEdge s da dz dx -> AnyEdge s da dz
          go (SomeForwardInnerEdge (ForwardEdge f head)) = AnyEdge (InnerTail name) f head

allFwdEdges :: forall s da dz. ForwardGraph s da dz -> [AnyEdge s da dz]
allFwdEdges (ForwardGraph env (ForwardFinalNode es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s da dz]
          innerEdges = concatMap nodeEdges (NodeMap.toList env)
            where nodeEdges (NodeMap.SomeItem name node) = forwardNodeEdges name node
          finalEdges :: [AnyEdge s da dz]
          finalEdges = go <$> es
            where go :: SomeForwardFinalEdge s da dz -> AnyEdge s da dz
                  go (SomeForwardFinalEdge (ForwardEdge f head)) = AnyEdge SinkTail f head

classifyBackEdge
  :: forall s da dz.
     AnyEdge s da dz
  -> Either (SomeBackwardInitialEdge s da dz) (SomeItem s (SomeBackwardInnerEdge s da dz))
classifyBackEdge (AnyEdge tail f head) = case head of
    SourceHead -> Left (SomeBackwardInitialEdge (BackwardEdge tail f))
    InnerHead x -> Right (SomeItem x (SomeBackwardInnerEdge (BackwardEdge tail f)))

data NodeDict dx = BasicVector dx => NodeDict

cvItemS :: SomeItem s f -> SomeItem s f
cvItemS (SomeItem x y) = SomeItem x y

backFromEdges :: forall s da dz. NodeSet s => NodeMap s NodeDict -> [AnyEdge s da dz] -> BackwardGraph s da dz
backFromEdges dictmap edges = BackwardGraph edgeMap (BackwardInitialNode initial)
    where (initial, inner) = partitionEithers (classifyBackEdge <$> edges)
          edgeList :: NodeMap s (List2 (SomeBackwardInnerEdge s da dz))
          edgeList = NodeMap.fromList [NodeMap.SomeItem xname (mkNode xname x) | (NodeMap.SomeItem xname x) <- (cvItemS <$> inner)]
            where mkNode :: NodeKey s dv -> SomeBackwardInnerEdge s da dz dv -> SomeBackwardInnerEdge s da dz dv
                  mkNode xname x = case (NodeMap.lookup dictmap xname) of
                      NodeDict -> x
          edgeMap :: NodeMap s (BackwardInnerNode s da dz)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (SomeBackwardInnerEdge s da dz) dx -> BackwardInnerNode s da dz dx
          withDict NodeDict (List2 xs) = BackwardInnerNode xs

mkdict :: ForwardGraph s da dz -> NodeMap s NodeDict
mkdict (ForwardGraph env _) = NodeMap.mapmap go env
    where go :: ForwardInnerNode s da dz dv -> NodeDict dv
          go = \case
            ForwardInnerNode _ -> NodeDict

flipGraph :: NodeSet s => ForwardGraph s da dz -> BackwardGraph s da dz
flipGraph fwd = backFromEdges (mkdict fwd) (allFwdEdges fwd)
