{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
module ExprWalker()
where
{-
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))
import Expr (Expr5(..), Edge'(Edge'), Endpoint' (SourceNode', InnerNode'))
import Sharing (TreeBuilder, BuildAction'(..))
import Data.Kind (Type)
import qualified Sharing
import Notensor (BasicVector, LinearEdge (identityFunc), FullVector)

data Node'' e a v = BasicVector v => Node'' [Edge''' e a v]

data CachedNode (e :: Type -> Type -> Type) a v where
    CachedSourceNode :: CachedNode e a a
    CachedClone :: NodeKey a v -> CachedNode e a v
    CachedInnerNode :: Node'' e a v -> CachedNode e a v


data NodeKey a v where
    InnerKey :: OpenKey v -> NodeKey a v

data Endpoint'' a v where
    SourceNode'' :: Endpoint'' a a
    InnerNode'' :: NodeKey a u -> Endpoint'' a v

data Edge'' e a v where
    Edge'' :: e u v -> NodeKey a u -> Edge'' e a v

data Edge''' e a v where
    Edge''' :: e u v -> Endpoint'' a u -> Edge''' e a v

data TailKey a v where
    SouceTailKey :: TailKey a a
    InnerTailKey :: NodeKey a a -> TailKey a v

-- shouldn't be called directly, only via Sharing.insertExpr
walkAncestors :: forall (e :: Type -> Type -> Type) a v. Expr5 e a v -> BuildAction' (CachedNode e a) v
walkAncestors = \case
    Expr5Var -> BuildAction' (return CachedSourceNode)
    Expr5 xs -> BuildAction' $ do
        ys <- traverse walkEdge xs
        return (CachedInnerNode (Node'' ys))

walkEdge :: Edge' e a v -> TreeBuilder (CachedNode e a) (Edge''' e a v)
walkEdge (Edge' f arg) =
    case arg of
        SourceNode' -> return (Edge''' f SourceNode'')
        InnerNode' g ->  do
            arg' <- walkNode g
            return (Edge''' f arg')

walkNode :: forall (e :: Type -> Type -> Type) a v. Expr5 e a v -> TreeBuilder (CachedNode e a) (Endpoint'' a v)
walkNode expr = do
    (k, _z) <- Sharing.insertExpr (walkAncestors expr) expr
    return (InnerKey k)

walkNode' :: forall (e :: Type -> Type -> Type) a v. Expr5 e a v -> TreeBuilder (CachedNode e a) (OpenKey v, CachedNode e a v)
walkNode' expr = do
    let sourceExpr = Expr5Var :: Expr5 e a a
    (k, _) <- Sharing.insertExpr (BuildAction' (return CachedSourceNode)) sourceExpr
    Sharing.insertExpr (walkAncestors expr) expr

walkNode'' :: forall (e :: Type -> Type -> Type) a v. Expr5 e a v -> TreeBuilder (CachedNode e a) (CachedNode e a v)
walkNode'' expr = do
    let sourceExpr = Expr5Var :: Expr5 e a a
    (k, _) <- Sharing.insertExpr (BuildAction' (return CachedSourceNode)) sourceExpr
    unBuildAction' (walkAncestors expr)

data CachedTree e a z = CachedTree (OpenMap (CachedNode e a)) (Node'' e a z)

data FinalNode e a z where
    FinalNode :: BasicVector z => [Edge' e a z] -> FinalNode e a z

runWalk :: FinalNode e a z -> IO (CachedTree e a z)
runWalk (FinalNode edges) = do
    (final_node, node_map) <- Sharing.runTreeBuilder (walkNode'' (Expr5 edges))
    case final_node of
        CachedInnerNode y -> return (CachedTree node_map y)

runWalk' :: forall e a z. (FullVector z, LinearEdge e) => Expr5 e a z -> IO (CachedTree e a z)
runWalk' x = runWalk (FinalNode [Edge' (identityFunc @e @z) (InnerNode' x)])
-}