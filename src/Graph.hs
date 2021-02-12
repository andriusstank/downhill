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
    , BackwardNode
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

data ForwardGraph s f da dz = ForwardGraph (NodeMap s (ForwardNode s f da dz)) (ForwardNode s f da dz dz)

--type BackwardEdge s da dz = ForwardEdge s BackFunction1 dz da

type BackwardNode s da dz = VectorSum (ForwardEdge s BackFunction1 dz da)

--data BackwardGraph s da dz = BackwardGraph (NodeMap s (BackwardNode s da dz)) (BackwardNode s da dz da)
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

evalBackMap :: forall s da dz. NodeMap s (BackwardNode s da dz) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. BackwardNode s da dz dx -> BackValue dx
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

classifyBackEdge'
  :: forall s f g da dz.
     (forall u v. f u v -> g v u)
  -> AnyEdge s f da dz
  -> Either (ForwardEdge s g dz da da) (SomeItem s (ForwardEdge s g dz da))
classifyBackEdge' flipFunc (AnyEdge tail f head) = case head of
    SourceHead -> Left (ForwardEdge (flipFunc f) tail)
    InnerHead x -> Right (SomeItem x (ForwardEdge (flipFunc f) tail))

data NodeDict dx = BasicVector dx => NodeDict

cvItemS :: SomeItem s f -> SomeItem s f
cvItemS (SomeItem x y) = SomeItem x y

backFromEdges
  :: forall s da dz. (NodeSet s, BasicVector da)
  => NodeMap s NodeDict
  -> [AnyEdge s AFunction1 da dz]
  -> BackwardGraph s da dz
backFromEdges dictmap edges = ForwardGraph edgeMap (VectorSum initial)
    where (initial, inner) = partitionEithers (classifyBackEdge' flipFunc1 <$> edges)
          edgeList :: NodeMap s (List2 (ForwardEdge s BackFunction1 dz da))
          edgeList = NodeMap.fromList [NodeMap.SomeItem xname (mkNode xname x) | (NodeMap.SomeItem xname x) <- (cvItemS <$> inner)]
            where mkNode :: NodeKey s dv -> ForwardEdge s BackFunction1 dz da dv -> ForwardEdge s BackFunction1 dz da dv
                  mkNode xname x = case (NodeMap.lookup dictmap xname) of
                      NodeDict -> x
          edgeMap :: NodeMap s (BackwardNode s da dz)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (ForwardEdge s BackFunction1 dz da) dx -> BackwardNode s da dz dx
          withDict NodeDict (List2 xs) = VectorSum xs

mkdict :: ForwardGraph s AFunction1 da dz -> NodeMap s NodeDict
mkdict (ForwardGraph env _) = NodeMap.mapmap go env
    where go :: ForwardNode s AFunction1 da dz dv -> NodeDict dv
          go = \case
            VectorSum _ -> NodeDict

flipGraph :: (NodeSet s, BasicVector da) => ForwardGraph s AFunction1 da dz -> BackwardGraph s da dz
flipGraph fwd = backFromEdges (mkdict fwd) (allFwdEdges fwd)
