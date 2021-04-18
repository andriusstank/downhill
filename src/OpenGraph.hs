{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    OpenGraph(..),
    runRecoverSharing4,
    --runRecoverSharing4',
    --runRecoverSharing6
)
where
import Expr(Expr5(Expr5, Expr5Var), LinearFunc5, Edge'(..))
import Sharing (BuildAction(BuildAction), TreeBuilder, BuildAction'(..))
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))
import ExprWalker
import qualified OpenMap
import Notensor (BasicVector)

type OpenArg = Endpoint OpenKey
type OpenTerm e = Edge OpenKey e
type OpenExpr e da = Node OpenKey e da

data InsertionResult g a v where
    NoInsert :: InsertionResult g a a
    DoInsert :: OpenKey v -> g v -> InsertionResult g a v

data InsertionResultArg a x v where
    NoInsertArg :: InsertionResultArg a a a
    DoInsertArg :: OpenArg a v -> InsertionResultArg a x v

data ExprResult e a v where
    NoInsertExpr :: ExprResult e a a
    DoInsertExpr :: TreeBuilder (OpenExpr e a) (OpenExpr e a v) -> ExprResult e a v

{-# DEPRECATED unInsertExprResult "remove" #-}

data InsertExprResult e a v where
    NoInsertInsertExpr :: InsertExprResult e a a
    DoInsertInsertExpr :: TreeBuilder (OpenExpr e a) (OpenKey v) -> InsertExprResult e a v

unInsertExprResult :: InsertExprResult e a v -> TreeBuilder (OpenExpr e a) (OpenArg a v)
unInsertExprResult = \case
    NoInsertInsertExpr -> return SourceNode
    DoInsertInsertExpr x -> InnerNode <$> x

insertExpr3 :: Expr5 e a v -> InsertExprResult e a v
insertExpr3 y = case goSharing4 y of
    NoInsertExpr -> NoInsertInsertExpr
    DoInsertExpr z -> DoInsertInsertExpr $ do
                (k, _zz) <- Sharing.insertExpr (BuildAction' z) y
                return k
goEdges :: BasicVector v => [Edge' e a v] -> TreeBuilder (OpenExpr e a) (Node OpenKey e a v)
goEdges xs = do
        let go' :: Edge' e a v -> TreeBuilder (OpenExpr e a) (OpenTerm e a v)
            go' = goSharing4term
        xs' <- traverse go' xs
        return $ Node xs'
    
goSharing4 :: Expr5 e a v -> ExprResult e a v
goSharing4 = \case
    Expr5Var -> NoInsertExpr
    Expr5 xs -> DoInsertExpr $ goEdges xs

goSharing4arg :: forall e da dv. Expr5 e da dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg y = do
    case insertExpr3 y of
        NoInsertInsertExpr -> return SourceNode 
        DoInsertInsertExpr z -> do
            gRef <- z
            return (InnerNode gRef)

goSharing4term :: forall e da dv. Edge' e da dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
goSharing4term = \case
    Edge' f arg -> do
        arg' <- goSharing4arg arg
        return (Edge f arg')

data OpenGraph e a z where
    TrivialOpenGraph :: OpenGraph e a a
    NontrivialOpenGraph :: Node OpenKey e a z -> OpenMap (OpenExpr e a) -> OpenGraph e a z

--data TwoGraphs e a z = TwoGraphs (NodeKey a z) (OpenMap (CachedNode e a)) (OpenMap (OpenExpr e a))

runRecoverSharing4'' :: forall e da dz. Expr5 e da dz -> IO (OpenGraph e da dz)
runRecoverSharing4'' = \case
    Expr5Var -> return TrivialOpenGraph
    Expr5 xs -> do
        let z = goEdges xs
        (final_node, graph) <- Sharing.runTreeBuilder z
        return (NontrivialOpenGraph final_node graph)

runRecoverSharing4 :: Expr5 e da dz -> IO (OpenGraph e da dz)
runRecoverSharing4 = runRecoverSharing4''
