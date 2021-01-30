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
    , Edge(..), SourceNode(..), SinkNode(..)
    , AnyHead(..), AnyTail(..), InnerNode(..)
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

{-
data PassthroughEdge a da z dz = AFunction a da z dz
data SourceEdge a da v dv = SourceEdge (ExprName v dv) (AFunction a da v dv)
data SinkEdge z dz u du = SinkEdge (AFunction u du z dz) (ExprName u du)
data InnerEdge u du v dv = InnerEdge (ExprName v dv) (AFunction u du v dv) (ExprName u du)
-}

--data ForwardNode a da v dv = ForwardNode [SourceEdge a da v dv] [InnerEdge ]

data SourceNode a da z dz dx where
    SourceNode :: SourceNode a da z dz da

data SinkNode a da z dz dx where
    SinkNode :: SinkNode a da z dz dz

data InnerNode s a da z dz dx where
    InnerNode :: NodeKey s dx -> InnerNode s a da z dz dx

data AnyHead s a da z dz dx where
    SourceHead :: SourceNode a da z dz dx -> AnyHead s a da z dz dx
    InnerHead :: InnerNode s a da z dz dx -> AnyHead s a da z dz dx

data AnyTail s a da z dz dx where
    SinkTail :: SinkNode a da z dz dx -> AnyTail s a da z dz dx
    InnerTail :: InnerNode s a da z dz dx -> AnyTail s a da z dz dx

data Edge head tail a da z dz du dv = Edge (tail dv) (AFunction1 du dv) (head du)

data SomeForwardInnerEdge s a da z dz dv = forall du. SomeForwardInnerEdge (Edge (AnyHead s a da z dz) (InnerNode s a da z dz) a da z dz du dv)
data SomeForwardFinalEdge s a da z dz = forall du. SomeForwardFinalEdge (Edge (AnyHead s a da z dz) (SinkNode a da z dz) a da z dz du dz)

data ForwardInnerNode s a da z dz dx where
    ForwardInnerNode :: BasicVector dx => [SomeForwardInnerEdge s a da z dz dx] -> ForwardInnerNode s a da z dz dx

data ForwardFinalNode s a da z dz = ForwardFinalNode (SinkNode a da z dz dz) [SomeForwardFinalEdge s a da z dz]

data ForwardGraph s a da z dz = ForwardGraph (NodeMap s (ForwardInnerNode s a da z dz)) (ForwardFinalNode s a da z dz)


data SomeBackwardInnerEdge s a da z dz du = forall dv. SomeBackwardInnerEdge (Edge (InnerNode s a da z dz) (AnyTail s a da z dz) a da z dz du dv)
data SomeBackwardInitialEdge s a da z dz = forall dv. SomeBackwardInitialEdge (Edge (SourceNode a da z dz) (AnyTail s a da z dz) a da z dz da dv)

data BackwardInnerNode s a da z dz dx where
    BackwardInnerNode :: BasicVector dx => [SomeBackwardInnerEdge s a da z dz dx] -> BackwardInnerNode s a da z dz dx

data BackwardInitialNode s a da z dz = BackwardInitialNode [SomeBackwardInitialEdge s a da z dz]

data BackwardGraph s a da z dz = BackwardGraph (NodeMap s (BackwardInnerNode s a da z dz)) (BackwardInitialNode s a da z dz)

data AnyEdge s a da z dz = forall du dv. AnyEdge (Edge (AnyHead s a da z dz) (AnyTail s a da z dz) a da z dz du dv)

convertGraph :: forall s a da z dz. NodeMap s (SharedExprS s a da) -> SharedExprS s a da dz -> ForwardGraph s a da z dz
convertGraph env (ExprSum zs) = ForwardGraph (NodeMap.mapmapWithKey convertInnerNode env) (ForwardFinalNode SinkNode (convertFinalEdge <$> zs))
    where convertInnerNode :: forall dx. NodeKey s dx -> SharedExprS s a da dx -> ForwardInnerNode s a da z dz dx
          convertInnerNode xname (ExprSum xs) = ForwardInnerNode (convertInnerEdge xname <$> xs)
          convertInnerEdge :: forall dx. NodeKey s dx -> SharedTermS s a da dx -> SomeForwardInnerEdge s a da z dz dx
          convertInnerEdge tail = \case
            Func2 f x -> SomeForwardInnerEdge (Edge (InnerNode tail) f (convertArg x))
          convertArg :: SharedArgS s a da du -> AnyHead s a da z dz du
          convertArg = \case
             ArgVar -> SourceHead SourceNode
             ArgExpr argName -> InnerHead (InnerNode argName)
          convertFinalEdge :: SharedTermS s a da dz -> SomeForwardFinalEdge s a da z dz
          convertFinalEdge = \case
            Func2 f x -> SomeForwardFinalEdge (Edge SinkNode f (convertArg x))

newtype BackValue dx = BackValue dx
    deriving Generic

goBackEdge' :: forall s head a da z dz du dv. BasicVector du => NodeMap s BackValue -> dz -> Edge head (AnyTail s a da z dz) a da z dz du dv -> VecBuilder du
goBackEdge' ys dz (Edge tail f _head) = backF1 f (goTail tail)
    where goTail :: AnyTail s a da z dz dx -> dx
          goTail = \case
            SinkTail tail' -> case tail' of
                SinkNode -> dz
            InnerTail tail' -> case tail' of
                InnerNode nodeName -> case NodeMap.lookup ys nodeName of
                    BackValue x -> x

evalBackMap :: forall s a da z dz. NodeMap s (BackwardInnerNode s a da z dz) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall dx. BackwardInnerNode s a da z dz dx -> BackValue dx
          go (BackwardInnerNode xs') = BackValue (sumBuilder (goEdge <$> xs'))
          goEdge :: BasicVector dx => SomeBackwardInnerEdge s a da z dz dx -> VecBuilder dx
          goEdge = \case
            SomeBackwardInnerEdge e -> goBackEdge' ys dz e

instance BasicVector da => TensorProduct dz (BackwardGraph s a da z dz) da where
    dx ⊗ BackwardGraph env (BackwardInitialNode edges) = sumBuilder (go <$> edges)
        where go :: SomeBackwardInitialEdge s a da z dz -> VecBuilder da
              go (SomeBackwardInitialEdge edge) = goBackEdge' env' dx edge
              env' = evalBackMap env dx

allFwdEdges :: forall s a da z dz. ForwardGraph s a da z dz -> [AnyEdge s a da z dz]
allFwdEdges (ForwardGraph env (ForwardFinalNode _ es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s a da z dz]
          innerEdges = concatMap nodeEdges (NodeMap.toList env)
            where nodeEdges (NodeMap.SomeItem _name (ForwardInnerNode xs)) = go <$> xs
                    where go :: SomeForwardInnerEdge s a da z dz dv -> AnyEdge s a da z dz
                          go (SomeForwardInnerEdge (Edge tail f head)) = AnyEdge (Edge (InnerTail tail) f head)
          finalEdges :: [AnyEdge s a da z dz]
          finalEdges = go <$> es
            where go :: SomeForwardFinalEdge s a da z dz -> AnyEdge s a da z dz
                  go (SomeForwardFinalEdge (Edge tail f head)) = AnyEdge (Edge (SinkTail tail) f head)

classifyBackEdge
  :: forall s a da z dz.
     AnyEdge s a da z dz
  -> Either (SomeBackwardInitialEdge s a da z dz) (SomeItem s (SomeBackwardInnerEdge s a da z dz))
classifyBackEdge (AnyEdge (Edge tail f head)) = case head of
    SourceHead x -> case x of
        SourceNode -> Left (SomeBackwardInitialEdge (Edge tail f SourceNode))
    InnerHead node@(InnerNode x) -> Right (SomeItem x (SomeBackwardInnerEdge (Edge tail f node)))

data NodeDict dx = BasicVector dx => NodeDict

cvItemS :: SomeItem s f -> SomeItem s f
cvItemS (SomeItem x y) = SomeItem x y

backFromEdges :: forall s a da z dz. NodeSet s => NodeMap s NodeDict -> [AnyEdge s a da z dz] -> BackwardGraph s a da z dz
backFromEdges dictmap edges = BackwardGraph edgeMap (BackwardInitialNode initial)
    where (initial, inner) = partitionEithers (classifyBackEdge <$> edges)
          edgeList :: NodeMap s (List2 (SomeBackwardInnerEdge s a da z dz))
          edgeList = NodeMap.fromList [NodeMap.SomeItem xname (mkNode xname x) | (NodeMap.SomeItem xname x) <- (cvItemS <$> inner)]
            where mkNode :: NodeKey s dv -> SomeBackwardInnerEdge s a da z dz dv -> SomeBackwardInnerEdge s a da z dz dv
                  mkNode xname x = case (NodeMap.lookup dictmap xname) of
                      NodeDict -> x
          edgeMap :: NodeMap s (BackwardInnerNode s a da z dz)
          edgeMap = NodeMap.zipWith withDict dictmap edgeList
          withDict :: NodeDict dx -> List2 (SomeBackwardInnerEdge s a da z dz) dx -> BackwardInnerNode s a da z dz dx
          withDict NodeDict (List2 xs) = BackwardInnerNode xs

mkdict :: ForwardGraph s a da z dz -> NodeMap s NodeDict
mkdict (ForwardGraph env _) = NodeMap.mapmap go env
    where go :: ForwardInnerNode s a da z dz dv -> NodeDict dv
          go = \case
            ForwardInnerNode _ -> NodeDict

flipGraph :: NodeSet s => ForwardGraph s a da z dz -> BackwardGraph s a da z dz
flipGraph fwd = backFromEdges (mkdict fwd) (allFwdEdges fwd)

data f :⊗ g :: Type -> Type -> Type -> Type -> Type where
    (:⊗) :: (LinearFunction (f x v dx dv) x v dx dv, LinearFunction (g u x du dx) u x du dx) => f x v dx dv -> g u x du dx -> (f :⊗ g) u v du dv

instance TensorProduct ((f :⊗ g) u v du dv) u v where
    (f :⊗ g) ⊗ x = f ⊗ (g ⊗ x)

instance TensorProduct dv ((f :⊗ g) u v du dv) du where
    x ⊗ (f :⊗ g) = (x ⊗ f) ⊗ g

instance
  ( AdditiveGroup u
  , AdditiveGroup v
  , AdditiveGroup du
  , AdditiveGroup dv
  ) => LinearFunction ((f :⊗ g) u v du dv) u v du dv where
