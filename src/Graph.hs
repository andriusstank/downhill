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

module Graph where
import Prelude hiding (head, tail)
import Sharing(ExprMap)
import Tensor(transposeFunc, LinearFunction, TensorProduct(..), AFunction(..))

import NodeMap (toExprName, unsafeNodeKey,  NodeMap, unsafeFromExprMap, toExprMap, NodeKey )
import qualified ExprRef as ExprMap
import Data.VectorSpace (sumV, AdditiveGroup)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Either (partitionEithers)
import NodeMap (SomeItem(..), NodeKey, SharedArgS(SharedArgExprS, SharedArgVarS), SharedExprS(..), SharedTermS(..))

import qualified NodeMap

{-
data PassthroughEdge a da z dz = AFunction a da z dz
data SourceEdge a da v dv = SourceEdge (ExprName v dv) (AFunction a da v dv)
data SinkEdge z dz u du = SinkEdge (AFunction u du z dz) (ExprName u du)
data InnerEdge u du v dv = InnerEdge (ExprName v dv) (AFunction u du v dv) (ExprName u du)
-}

--data ForwardNode a da v dv = ForwardNode [SourceEdge a da v dv] [InnerEdge ]

data SourceNode a da z dz x dx where
    SourceNode :: SourceNode a da z dz a da

data SinkNode a da z dz x dx where
    SinkNode :: SinkNode a da z dz z dz

data InnerNode s a da z dz x dx where
    InnerNode :: NodeKey s x dx -> InnerNode s a da z dz x dx

data AnyHead s a da z dz x dx where
    SourceHead :: SourceNode a da z dz x dx -> AnyHead s a da z dz x dx
    InnerHead :: InnerNode s a da z dz x dx -> AnyHead s a da z dz x dx

data AnyTail s a da z dz x dx where
    SinkTail :: SinkNode a da z dz x dx -> AnyTail s a da z dz x dx
    InnerTail :: InnerNode s a da z dz x dx -> AnyTail s a da z dz x dx

data Edge head tail a da z dz u du v dv = Edge (tail a da z dz v dv) (AFunction u du v dv) (head a da z dz u du)

{-
type ForwardInnerEdge = Edge AnyHead InnerNode
type ForwardFinalEdge = Edge AnyHead SinkNode
type BackInitialEdge = Edge SourceNode AnyTail
type BackInnerEdge = Edge InnerNode AnyTail
-}

data ForwardEdge s a da z dz u du v dv
    = ForwardFinalEdge (Edge (AnyHead s) SinkNode a da z dz u du v dv)
    | ForwardInnerEdge (Edge (AnyHead s) (InnerNode s) a da z dz u du v dv)

data BackwardEdge s a da z dz u du v dv
    = BackInnerEdge (Edge (InnerNode s) (AnyTail s) a da z dz u du v dv)
    | BackInitialEdge (Edge SourceNode (AnyTail s) a da z dz u du v dv)

classifyForwardInnerEdge :: Edge (AnyHead s) (InnerNode s) a da z dz u du v dv -> BackwardEdge s a da z dz u du v dv
classifyForwardInnerEdge (Edge tail f head) = case head of
    SourceHead head' -> BackInitialEdge (Edge (InnerTail tail) f head')
    InnerHead head' -> BackInnerEdge (Edge (InnerTail tail) f head')

classifyForwardFinalEdge :: Edge (AnyHead s) SinkNode a da z dz u du v dv -> BackwardEdge s a da z dz u du v dv
classifyForwardFinalEdge (Edge tail f head) = case head of
    SourceHead head' -> BackInitialEdge (Edge (SinkTail tail) f head')
    InnerHead head' -> BackInnerEdge (Edge (SinkTail tail) f head')

classifyBackwardInnerEdge :: Edge (InnerNode s) (AnyTail s) a da z dz u du v dv -> ForwardEdge s a da z dz u du v dv
classifyBackwardInnerEdge (Edge tail f head) = case tail of
    SinkTail tail' -> ForwardFinalEdge (Edge tail' f (InnerHead head))
    InnerTail tail' -> ForwardInnerEdge (Edge tail' f (InnerHead head))

classifyBackwardInitialEdge :: Edge SourceNode (AnyTail s) a da z dz u du v dv -> ForwardEdge s a da z dz u du v dv
classifyBackwardInitialEdge (Edge tail f head) = case tail of
    SinkTail tail' -> ForwardFinalEdge (Edge tail' f (SourceHead head))
    InnerTail tail' -> ForwardInnerEdge (Edge tail' f (SourceHead head))

data SomeForwardInnerEdge s a da z dz v dv = forall u du. SomeForwardInnerEdge (Edge (AnyHead s) (InnerNode s) a da z dz u du v dv)
data SomeForwardFinalEdge s a da z dz = forall u du. SomeForwardFinalEdge (Edge (AnyHead s) SinkNode a da z dz u du z dz)

data ForwardInnerNode s a da z dz x dx where
    ForwardInnerNode :: (AdditiveGroup x, AdditiveGroup dx) => [SomeForwardInnerEdge s a da z dz x dx] -> ForwardInnerNode s a da z dz x dx

data ForwardFinalNode s a da z dz = ForwardFinalNode (SinkNode a da z dz z dz) [SomeForwardFinalEdge s a da z dz]
data SomeForwardInnerNode s a da z dz = forall x dx. SomeForwardInnerNode [SomeForwardInnerEdge s a da z dz x dx]

data ForwardGraph s a da z dz = ForwardGraph (NodeMap s (ForwardInnerNode s a da z dz)) (ForwardFinalNode s a da z dz)


data SomeBackwardInnerEdge s a da z dz u du = forall v dv. SomeBackwardInnerEdge (Edge (InnerNode s) (AnyTail s) a da z dz u du v dv)
data SomeBackwardInitialEdge s a da z dz = forall v dv. SomeBackwardInitialEdge (Edge SourceNode (AnyTail s) a da z dz a da v dv)

data BackwardInnerNode s a da z dz x dx where
    BackwardInnerNode :: AdditiveGroup dx => [SomeBackwardInnerEdge s a da z dz x dx] -> BackwardInnerNode s a da z dz x dx

data BackwardInitialNode s a da z dz = BackwardInitialNode [SomeBackwardInitialEdge s a da z dz]

data BackwardGraph s a da z dz = BackwardGraph (NodeMap s (BackwardInnerNode s a da z dz)) (BackwardInitialNode s a da z dz)

data AnyEdge s a da z dz = forall u du v dv. AnyEdge (Edge (AnyHead s) (AnyTail s) a da z dz u du v dv)

convertGraph :: forall s a da z dz. NodeMap s (SharedExprS s a da) -> SharedExprS s a da z dz -> ForwardGraph s a da z dz
convertGraph env zs' = ForwardGraph (NodeMap.mapmapWithKey convertInnerNode env) (ForwardFinalNode SinkNode (convertFinalEdge <$> zs))
    where convertInnerNode :: forall x dx. NodeKey s x dx -> SharedExprS s a da x dx -> ForwardInnerNode s a da z dz x dx
          convertInnerNode xname (SharedExprSumS xs) = ForwardInnerNode (convertInnerEdge xname <$> xs)
          convertInnerEdge :: forall x dx. NodeKey s x dx -> SharedTermS s a da x dx -> SomeForwardInnerEdge s a da z dz x dx
          convertInnerEdge tail = \case
            SharedFunApS f x -> SomeForwardInnerEdge (Edge (InnerNode tail) f (convertArg x))
          convertArg :: SharedArgS s a da u du -> AnyHead s a da z dz u du
          convertArg = \case
             SharedArgVarS -> SourceHead SourceNode
             SharedArgExprS argName -> InnerHead (InnerNode argName)
          convertFinalEdge :: SharedTermS s a da z dz -> SomeForwardFinalEdge s a da z dz
          convertFinalEdge = \case
            SharedFunApS f x -> SomeForwardFinalEdge (Edge SinkNode f (convertArg x))
          zs = unSharedExprSumS zs'

newtype FwdValue x dx = FwdValue x
    deriving Generic
newtype BackValue x dx = BackValue dx
    deriving Generic

instance AdditiveGroup x => AdditiveGroup (FwdValue x dx)
instance AdditiveGroup dx => AdditiveGroup (BackValue x dx)

goEdge' :: forall s tail a da z dz u du v dv. NodeMap s FwdValue -> a -> Edge (AnyHead s) tail a da z dz u du v dv -> v
goEdge' ys a (Edge _tail f head) = f ⊗ goHead head
    where goHead :: AnyHead s a da z dz x dx -> x
          goHead = \case
            SourceHead head' -> case head' of
                SourceNode -> a
            InnerHead head' -> case head' of
                InnerNode nodeName -> case NodeMap.lookup ys nodeName of
                    FwdValue x -> x

evalFwdMap :: forall s a da z dz. NodeMap s (ForwardInnerNode s a da z dz) -> a -> NodeMap s FwdValue
evalFwdMap xs a = ys
    where ys = NodeMap.mapmap go xs
          go :: forall x dx. ForwardInnerNode s a da z dz x dx -> FwdValue x dx
          go (ForwardInnerNode xs') = sumV (goEdge <$> xs')
          goEdge :: SomeForwardInnerEdge s a da z dz x dx -> FwdValue x dx
          goEdge = \case
            SomeForwardInnerEdge e -> FwdValue (goEdge' ys a e)

goBackEdge' :: forall s head a da z dz u du v dv. NodeMap s BackValue -> dz -> Edge head (AnyTail s) a da z dz u du v dv -> du
goBackEdge' ys dz (Edge tail f _head) = goTail tail ⊗ f
    where goTail :: AnyTail s a da z dz x dx -> dx
          goTail = \case
            SinkTail tail' -> case tail' of
                SinkNode -> dz
            InnerTail tail' -> case tail' of
                InnerNode nodeName -> case NodeMap.lookup ys nodeName of
                    BackValue x -> x

evalBackMap :: forall s a da z dz. NodeMap s (BackwardInnerNode s a da z dz) -> dz -> NodeMap s BackValue
evalBackMap dxs dz = ys
    where ys = NodeMap.mapmap go dxs
          go :: forall x dx. BackwardInnerNode s a da z dz x dx -> BackValue x dx
          go (BackwardInnerNode xs') = sumV (goEdge <$> xs')
          goEdge :: SomeBackwardInnerEdge s a da z dz x dx -> BackValue x dx
          goEdge = \case
            SomeBackwardInnerEdge e -> BackValue (goBackEdge' ys dz e)

instance AdditiveGroup z => TensorProduct (ForwardGraph s a da z dz) a z where
    ForwardGraph env (ForwardFinalNode _sink edges) ⊗ x = sumV (go <$> edges)
        where go :: SomeForwardFinalEdge s a da z dz -> z
              go (SomeForwardFinalEdge edge) = goEdge' env' x edge
              env' = evalFwdMap env x

instance AdditiveGroup da => TensorProduct dz (BackwardGraph s a da z dz) da where
    dx ⊗ BackwardGraph env (BackwardInitialNode edges) = sumV (go <$> edges)
        where go :: SomeBackwardInitialEdge s a da z dz -> da
              go (SomeBackwardInitialEdge edge) = goBackEdge' env' dx edge
              env' = evalBackMap env dx

allFwdEdges :: forall s a da z dz. ForwardGraph s a da z dz -> [AnyEdge s a da z dz]
allFwdEdges (ForwardGraph env (ForwardFinalNode _ es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge s a da z dz]
          innerEdges = concatMap nodeEdges (NodeMap.toList env)
            where nodeEdges (NodeMap.SomeItem _name (ForwardInnerNode xs)) = go <$> xs
                    where go :: SomeForwardInnerEdge s a da z dz v dv -> AnyEdge s a da z dz
                          go (SomeForwardInnerEdge (Edge tail f head)) = AnyEdge (Edge (InnerTail tail) f head)
          finalEdges :: [AnyEdge s a da z dz]
          finalEdges = go <$> es
            where go :: SomeForwardFinalEdge s a da z dz -> AnyEdge s a da z dz
                  go (SomeForwardFinalEdge (Edge tail f head)) = AnyEdge (Edge (SinkTail tail) f head)

--data SomeExprWithNameS s f = forall v dv. SomeExprWithNameS (NodeKey s v dv) (f v dv)
{-# DEPRECATED SomeExprWithNameS "transitionary" #-}
type SomeExprWithNameS = SomeItem

classifyBackEdge
  :: forall s a da z dz.
     AnyEdge s a da z dz
  -> Either (SomeBackwardInitialEdge s a da z dz) (SomeExprWithNameS s (SomeBackwardInnerEdge s a da z dz))
classifyBackEdge (AnyEdge (Edge tail f head)) = case head of
    SourceHead x -> case x of
        SourceNode -> Left (SomeBackwardInitialEdge (Edge tail f SourceNode))
    InnerHead node@(InnerNode x) -> Right (SomeItem x (SomeBackwardInnerEdge (Edge tail f node)))

data NodeDict x dx = (AdditiveGroup x, AdditiveGroup dx) => NodeDict

cvItemS :: SomeExprWithNameS s f -> SomeItem s f
cvItemS (SomeItem x y) = SomeItem x y

backFromEdges :: forall s a da z dz. NodeMap s NodeDict -> [AnyEdge s a da z dz] -> BackwardGraph s a da z dz
backFromEdges dictmap edges = BackwardGraph edgeMap (BackwardInitialNode initial)
    where (initial, inner) = partitionEithers (classifyBackEdge <$> edges)
          edgeMap :: NodeMap s (BackwardInnerNode s a da z dz)
          edgeMap = NodeMap.fromListWith [NodeMap.SomeItem xname (mkNode xname x) | (NodeMap.SomeItem xname x) <- (cvItemS <$> inner)] addb
            where mkNode :: NodeKey s v dv -> SomeBackwardInnerEdge s a da z dz v dv -> BackwardInnerNode s a da z dz v dv
                  mkNode xname x = case (NodeMap.lookup dictmap xname) of
                      NodeDict -> BackwardInnerNode [x]
          addb :: BackwardInnerNode s a da z dz x dx -> BackwardInnerNode s a da z dz x dx -> BackwardInnerNode s a da z dz x dx
          addb (BackwardInnerNode xs) (BackwardInnerNode ys) = BackwardInnerNode (xs ++ ys)

mkdict :: ForwardGraph s a da z dz -> NodeMap s NodeDict
mkdict (ForwardGraph env _) = NodeMap.mapmap go env
    where go :: ForwardInnerNode s a da z dz v dv -> NodeDict v dv
          go = \case
            ForwardInnerNode _ -> NodeDict

flipGraph :: ForwardGraph s a da z dz -> BackwardGraph s a da z dz
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
