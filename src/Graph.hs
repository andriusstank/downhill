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
import ExprRef (ExprMap, ExprName, SomeExprWithName(..), SomeExpr(..))
import Sharing(SharedTerm(..), SharedExpr(..), SharedArg(..))
import Tensor(transposeFunc, LinearFunction, TensorProduct(..), AFunction(..))

import qualified ExprRef as ExprMap
import Data.VectorSpace (sumV, AdditiveGroup)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Either (partitionEithers)

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

data InnerNode a da z dz x dx where
    InnerNode :: ExprName x dx -> InnerNode a da z dz x dx

data AnyHead a da z dz x dx where
    SourceHead :: SourceNode a da z dz x dx -> AnyHead a da z dz x dx
    InnerHead :: InnerNode a da z dz x dx -> AnyHead a da z dz x dx

data AnyTail a da z dz x dx where
    SinkTail :: SinkNode a da z dz x dx -> AnyTail a da z dz x dx
    InnerTail :: InnerNode a da z dz x dx -> AnyTail a da z dz x dx

data Edge head tail a da z dz u du v dv = Edge (tail a da z dz v dv) (AFunction u du v dv) (head a da z dz u du)

{-
type ForwardInnerEdge = Edge AnyHead InnerNode
type ForwardFinalEdge = Edge AnyHead SinkNode
type BackInitialEdge = Edge SourceNode AnyTail
type BackInnerEdge = Edge InnerNode AnyTail
-}

data ForwardEdge a da z dz u du v dv
    = ForwardFinalEdge (Edge AnyHead SinkNode a da z dz u du v dv)
    | ForwardInnerEdge (Edge AnyHead InnerNode a da z dz u du v dv)

data BackwardEdge a da z dz u du v dv
    = BackInnerEdge (Edge InnerNode AnyTail a da z dz u du v dv)
    | BackInitialEdge (Edge SourceNode AnyTail a da z dz u du v dv)

classifyForwardInnerEdge :: Edge AnyHead InnerNode a da z dz u du v dv -> BackwardEdge a da z dz u du v dv
classifyForwardInnerEdge (Edge tail f head) = case head of
    SourceHead head' -> BackInitialEdge (Edge (InnerTail tail) f head')
    InnerHead head' -> BackInnerEdge (Edge (InnerTail tail) f head')

classifyForwardFinalEdge :: Edge AnyHead SinkNode a da z dz u du v dv -> BackwardEdge a da z dz u du v dv
classifyForwardFinalEdge (Edge tail f head) = case head of
    SourceHead head' -> BackInitialEdge (Edge (SinkTail tail) f head')
    InnerHead head' -> BackInnerEdge (Edge (SinkTail tail) f head')

classifyBackwardInnerEdge :: Edge InnerNode AnyTail a da z dz u du v dv -> ForwardEdge a da z dz u du v dv
classifyBackwardInnerEdge (Edge tail f head) = case tail of
    SinkTail tail' -> ForwardFinalEdge (Edge tail' f (InnerHead head))
    InnerTail tail' -> ForwardInnerEdge (Edge tail' f (InnerHead head))

classifyBackwardInitialEdge :: Edge SourceNode AnyTail a da z dz u du v dv -> ForwardEdge a da z dz u du v dv
classifyBackwardInitialEdge (Edge tail f head) = case tail of
    SinkTail tail' -> ForwardFinalEdge (Edge tail' f (SourceHead head))
    InnerTail tail' -> ForwardInnerEdge (Edge tail' f (SourceHead head))

data SomeForwardInnerEdge a da z dz v dv = forall u du. SomeForwardInnerEdge (Edge AnyHead InnerNode a da z dz u du v dv)
data SomeForwardFinalEdge a da z dz = forall u du. SomeForwardFinalEdge (Edge AnyHead SinkNode a da z dz u du z dz)

data ForwardInnerNode a da z dz x dx where
    ForwardInnerNode :: (AdditiveGroup x, AdditiveGroup dx) => [SomeForwardInnerEdge a da z dz x dx] -> ForwardInnerNode a da z dz x dx

data ForwardFinalNode a da z dz = ForwardFinalNode (SinkNode a da z dz z dz) [SomeForwardFinalEdge a da z dz]
data SomeForwardInnerNode a da z dz = forall x dx. SomeForwardInnerNode [SomeForwardInnerEdge a da z dz x dx]

data ForwardGraph a da z dz = ForwardGraph (ExprMap (ForwardInnerNode a da z dz)) (ForwardFinalNode a da z dz)


data SomeBackwardInnerEdge a da z dz u du = forall v dv. SomeBackwardInnerEdge (Edge InnerNode AnyTail a da z dz u du v dv)
data SomeBackwardInitialEdge a da z dz = forall v dv. SomeBackwardInitialEdge (Edge SourceNode AnyTail a da z dz a da v dv)

data BackwardInnerNode a da z dz x dx where
    BackwardInnerNode :: AdditiveGroup dx => [SomeBackwardInnerEdge a da z dz x dx] -> BackwardInnerNode a da z dz x dx

data BackwardInitialNode a da z dz = forall x dx. BackwardInitialNode [SomeBackwardInitialEdge a da z dz]

data BackwardGraph a da z dz = BackwardGraph (ExprMap (BackwardInnerNode a da z dz)) (BackwardInitialNode a da z dz)

data AnyEdge a da z dz = forall u du v dv. AnyEdge (Edge AnyHead AnyTail a da z dz u du v dv)

convertGraph :: forall a da z dz. ExprMap (SharedExpr a da) -> SharedExpr a da z dz -> ForwardGraph a da z dz
convertGraph env (SharedExprSum zs) = ForwardGraph (ExprMap.mapmapWithKey convertInnerNode env) (ForwardFinalNode SinkNode (convertFinalEdge <$> zs))
    where convertInnerNode :: forall x dx. ExprName x dx -> SharedExpr a da x dx -> ForwardInnerNode a da z dz x dx
          convertInnerNode xname (SharedExprSum xs) = ForwardInnerNode (convertInnerEdge xname <$> xs)
          convertInnerEdge :: forall x dx. ExprName x dx -> SharedTerm a da x dx -> SomeForwardInnerEdge a da z dz x dx
          convertInnerEdge tail = \case
            SharedFunAp f x -> SomeForwardInnerEdge (Edge (InnerNode tail) f (convertArg x))
          convertArg :: SharedArg a da u du -> AnyHead a da z dz u du
          convertArg = \case
             SharedArgVar -> SourceHead SourceNode
             SharedArgExpr argName -> InnerHead (InnerNode argName)
          convertFinalEdge :: SharedTerm a da z dz -> SomeForwardFinalEdge a da z dz
          convertFinalEdge = \case
            SharedFunAp f x -> SomeForwardFinalEdge (Edge SinkNode f (convertArg x))

newtype FwdValue x dx = FwdValue x
    deriving Generic
newtype BackValue x dx = BackValue dx
    deriving Generic

instance AdditiveGroup x => AdditiveGroup (FwdValue x dx)
instance AdditiveGroup dx => AdditiveGroup (BackValue x dx)

goEdge' :: forall tail a da z dz u du v dv. ExprMap FwdValue -> a -> Edge AnyHead tail a da z dz u du v dv -> v
goEdge' ys a (Edge _tail f head) = f ⊗ goHead head
    where goHead :: AnyHead a da z dz x dx -> x
          goHead = \case
            SourceHead head' -> case head' of
                SourceNode -> a
            InnerHead head' -> case head' of
                InnerNode nodeName -> case ExprMap.lookupExprName ys nodeName of
                    Nothing -> error "oh fuck"
                    Just (FwdValue x) -> x

evalFwdMap :: forall a da z dz. ExprMap (ForwardInnerNode a da z dz) -> a -> ExprMap FwdValue
evalFwdMap xs a = ys
    where ys = ExprMap.mapmap go xs
          go :: forall x dx. ForwardInnerNode a da z dz x dx -> FwdValue x dx
          go (ForwardInnerNode xs') = sumV (goEdge <$> xs')
          goEdge :: SomeForwardInnerEdge a da z dz x dx -> FwdValue x dx
          goEdge = \case
            SomeForwardInnerEdge e -> FwdValue (goEdge' ys a e)

goBackEdge' :: forall head a da z dz u du v dv. ExprMap BackValue -> dz -> Edge head AnyTail a da z dz u du v dv -> du
goBackEdge' ys dz (Edge tail f _head) = goTail tail ⊗ f
    where goTail :: AnyTail a da z dz x dx -> dx
          goTail = \case
            SinkTail tail' -> case tail' of
                SinkNode -> dz
            InnerTail tail' -> case tail' of
                InnerNode nodeName -> case ExprMap.lookupExprName ys nodeName of
                    Nothing -> error "oh fuck"
                    Just (BackValue x) -> x

evalBackMap :: forall a da z dz. ExprMap (BackwardInnerNode a da z dz) -> dz -> ExprMap BackValue
evalBackMap dxs dz = ys
    where ys = ExprMap.mapmap go dxs
          go :: forall x dx. BackwardInnerNode a da z dz x dx -> BackValue x dx
          go (BackwardInnerNode xs') = sumV (goEdge <$> xs')
          goEdge :: SomeBackwardInnerEdge a da z dz x dx -> BackValue x dx
          goEdge = \case
            SomeBackwardInnerEdge e -> BackValue (goBackEdge' ys dz e)

instance AdditiveGroup z => TensorProduct (ForwardGraph a da z dz) a z where
    ForwardGraph env (ForwardFinalNode _sink edges) ⊗ x = sumV (go <$> edges)
        where go :: SomeForwardFinalEdge a da z dz -> z
              go (SomeForwardFinalEdge edge) = goEdge' env' x edge
              env' = evalFwdMap env x

instance AdditiveGroup da => TensorProduct dz (BackwardGraph a da z dz) da where
    dx ⊗ BackwardGraph env (BackwardInitialNode edges) = sumV (go <$> edges)
        where go :: SomeBackwardInitialEdge a da z dz -> da
              go (SomeBackwardInitialEdge edge) = goBackEdge' env' dx edge
              env' = evalBackMap env dx

allFwdEdges :: forall a da z dz. ForwardGraph a da z dz -> [AnyEdge a da z dz]
allFwdEdges (ForwardGraph env (ForwardFinalNode _ es)) = finalEdges ++ innerEdges
    where innerEdges :: [AnyEdge a da z dz]
          innerEdges = concatMap nodeEdges (ExprMap.toList env)
            where nodeEdges (SomeExprWithName _name (ForwardInnerNode xs)) = go <$> xs
                    where go :: SomeForwardInnerEdge a da z dz v dv -> AnyEdge a da z dz
                          go (SomeForwardInnerEdge (Edge tail f head)) = AnyEdge (Edge (InnerTail tail) f head)
          finalEdges :: [AnyEdge a da z dz]
          finalEdges = go <$> es
            where go :: SomeForwardFinalEdge a da z dz -> AnyEdge a da z dz
                  go (SomeForwardFinalEdge (Edge tail f head)) = AnyEdge (Edge (SinkTail tail) f head)

classifyBackEdge
  :: forall a da z dz.
     AnyEdge a da z dz
  -> Either (SomeBackwardInitialEdge a da z dz) (SomeExprWithName (SomeBackwardInnerEdge a da z dz))
classifyBackEdge (AnyEdge (Edge tail f head)) = case head of
    SourceHead x -> case x of
        SourceNode -> Left (SomeBackwardInitialEdge (Edge tail f SourceNode))
    InnerHead node@(InnerNode x) -> Right (SomeExprWithName x (SomeBackwardInnerEdge (Edge tail f node)))

data NodeDict x dx = (AdditiveGroup x, AdditiveGroup dx) => NodeDict

backFromEdges :: forall a da z dz. ExprMap NodeDict -> [AnyEdge a da z dz] -> BackwardGraph a da z dz
backFromEdges dictmap edges = BackwardGraph edgeMap (BackwardInitialNode initial)
    where (initial, inner) = partitionEithers (classifyBackEdge <$> edges)
          edgeMap :: ExprMap (BackwardInnerNode a da z dz)
          edgeMap = ExprMap.fromListWith [SomeExprWithName xname (mkNode xname x) | (SomeExprWithName xname x) <- inner] addb
            where mkNode :: ExprName v dv -> SomeBackwardInnerEdge a da z dz v dv -> BackwardInnerNode a da z dz v dv
                  mkNode xname x = case (ExprMap.lookupExprName dictmap xname) of
                      Nothing -> error "oh fuck"
                      Just NodeDict -> BackwardInnerNode [x]
          addb :: BackwardInnerNode a da z dz x dx -> BackwardInnerNode a da z dz x dx -> BackwardInnerNode a da z dz x dx
          addb (BackwardInnerNode xs) (BackwardInnerNode ys) = BackwardInnerNode (xs ++ ys)

mkdict :: ForwardGraph a da z dz -> ExprMap NodeDict
mkdict (ForwardGraph env _) = ExprMap.mapmap go env
    where go :: ForwardInnerNode a da z dz v dv -> NodeDict v dv
          go = \case
            ForwardInnerNode _ -> NodeDict

flipGraph :: ForwardGraph a da z dz -> BackwardGraph a da z dz
flipGraph fwd = backFromEdges (mkdict fwd) (allFwdEdges fwd)
-- data ForwardNode v dv = ForwardNode (ExprName v dv) 

--data Edge a da z dz head tail where
--    Edge :: tail z dz v dv -> AFunction u du v dv -> head a da u du -> Edge a da z dz head tail

--data OutEdge z dz a da where
--    OutEdge :: EdgeTail z dz v dv -> AFunction u du v dv -> EdgeHead a da u du -> OutEdge z dz a da

{-

data OutEdges z dz where
    OutEdges :: [OutEdge z dz a da] -> OutEdges z dz

flipEdge :: Edge a da z dz -> OutEdge z dz a da
flipEdge (Edge tail f head) = OutEdge tail f head

allEdges :: forall a da z dz. ExprMap (SharedExpr a da) -> SharedExpr a da z dz -> [Edge a da z dz]
allEdges = \env sink -> finalEdges sink ++ innerEdges env
    where 
          innerEdges :: ExprMap (SharedExpr a da) -> [Edge a da z dz]
          innerEdges = concatMap go . ExprMap.toList
              where go :: SomeExprWithName (SharedExpr a da) -> [Edge a da z dz]
                    go = \case
                        SomeExprWithName xname (SharedExprSum xs) -> map (makeEdge (EdgeTail xname)) xs

          finalEdges :: SharedExpr a da z dz -> [Edge a da z dz]
          finalEdges (SharedExprSum xs) = map (makeEdge Sink) xs

          makeEdge :: forall v dv. EdgeTail z dz v dv -> SharedTerm a da v dv -> Edge a da z dz
          makeEdge tail (SharedFunAp f x) = Edge tail f head
              where head = case x of
                      SharedArgVar -> Source
                      SharedArgExpr x' -> EdgeHead x'

flipEdges :: [Edge a da z dz] -> [OutEdge z dz a da]
flipEdges = map flipEdge
-}
--rebuildGraph :: [OutEdge z dz a da] -> (ExprMap OutEdges, OutEdges z dz)
--rebuildGraph edges = (ExprMap.fromListWith _ _, _initEdges)

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

{-
data LeftEndpoint z dz v dv where
    LeftVar :: LeftEndpoint z dz z dz
    LeftExpr :: BackNode z dz v dv -> LeftEndpoint z dz v dv

data LeftEdge z dz u du where
    BackEdge :: LeftEndpoint z dz v dv -> AFunction u du v dv -> LeftEdge z dz u du

newtype BackNode z dz v dv = BackNode [LeftEdge z dz v dv]


flipArg :: SharedArg a da v dv -> LeftEndpoint z dz v dv
flipArg = \case
    SharedArgVar -> 
-}
{-
data RightEndpoint b a da z dz v dv where
    RightVar :: RightEndpoint b a da z dz a da
    RightExpr :: SharedBiNode b a da z dz v dv -> RightEndpoint b a da z dz v dv


data FwdEdge b a da z dz v dv where
    FwdEdge :: AFunction b u du v dv -> RightEndpoint b a da z dz u du -> FwdEdge b a da z dz v dv


data InEdges b a da z dz v dv = (AdditiveGroup v, AdditiveGroup dv) => InEdges
    { shInEdges :: [FwdEdge b a da z dz v dv]
    }

data OutEdges b a da z dz v dv = (AdditiveGroup v, AdditiveGroup dv) => OutEdges
    { shOutEdges :: [BackEdge b a da z dz v dv]
    }

data SharedBiExpr b a da z dz v dv = SharedBiExpr (InEdges b a da z dz v dv) (OutEdges b a da z dz v dv)

instance TensorProduct (RightEndpoint b a da z dz v dv) a v where
    f ⊗ x = case f of
        RightVar -> x
        RightExpr f' -> _ -- f' ⊗ x

instance TensorProduct dz (LeftEndpoint b a da z dz v dv) dv where
    x ⊗ f = case f of
        LeftVar -> x
        LeftExpr f' -> _ --x ⊗ f'

instance TensorProduct (FwdEdge b a da z dz v dv) a v where
    FwdEdge f g ⊗ x = f ⊗ (g ⊗ x)

instance TensorProduct dz (BackEdge b a da z dz u du) du where
    dx ⊗ BackEdge f g = (dx ⊗ f) ⊗ g

instance TensorProduct (InEdges b a da z dz v dv) a v where
    e ⊗ x = case e of
        InEdges fs -> sumV [ f ⊗ x | f <- fs]

instance TensorProduct dz (OutEdges b a da z dz v dv) dv where
    x ⊗ e = case e of
        OutEdges fs -> sumV [x ⊗ f | f <- fs ]
-}

--meetInTheMiddle :: TensorProduct dv v b => dz -> SharedBiExpr b a da z dz v dv -> a -> b
--meetInTheMiddle dz (SharedBiExpr f g) a = (dz ⊗ f) ⊗ (g ⊗ a)

