{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    OpenGraph(..),
    --runRecoverSharing4,
    runRecoverSharing4'
)
where
import Expr(Expr5(Expr5, Expr5Subs), LinearFunc5, Edge'(..), Endpoint'(..))
import Sharing (BuildAction(BuildAction), TreeBuilder, BuildAction'(..))
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))

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

unExprResult :: ExprResult e a v -> TreeBuilder (OpenExpr e a) (OpenExpr e a v)
unExprResult = \case
    DoInsertExpr x -> x

insertExpr3 :: OpenArg da dx -> Expr5 e dx dv -> TreeBuilder (OpenExpr e da) (OpenKey dv, OpenExpr e da dv)
insertExpr3 x y = do
    (k, z) <- Sharing.insertExpr (BuildAction' (unExprResult $ goSharing4 x y)) y
    return (k, z)

goSharing4 :: forall e x a v. OpenArg a x -> Expr5 e x v -> ExprResult e a v
goSharing4 src = \case
    Expr5 xs -> DoInsertExpr $ do
        let go' :: Edge' e x v -> TreeBuilder (OpenExpr e a) (OpenTerm e a v)
            go' = goSharing4term src
        xs' <- traverse go' xs
        return $ Node xs'
    Expr5Subs f g -> DoInsertExpr $ do
        case f of
            SourceNode' -> unExprResult $ goSharing4 src g
            InnerNode' f' -> do
                (gRef, _sg) <- insertExpr3 src g
                unExprResult $ goSharing4 (InnerNode gRef) f'

goSharing4arg :: forall e dx da dv. OpenArg da dx -> Endpoint' e dx dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg src = \case
    SourceNode' -> return src
    InnerNode' g ->  do
        (gRef, _sg) <- insertExpr3 src g
        return (InnerNode gRef)

goSharing4term :: forall e dx da dv. OpenArg da dx -> Edge' e dx dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
goSharing4term src = \case
    Edge' f arg -> do
        arg' <- goSharing4arg src arg
        return (Edge f arg')

data OpenGraph e a z where
    TrivialOpenGraph :: OpenGraph e a a
    NontrivialOpenGraph :: Node OpenKey e a z -> OpenMap (OpenExpr e a) -> OpenGraph e a z

runRecoverSharing4' :: forall e da dz. LinearFunc5 e da dz -> IO (OpenGraph e da dz)
runRecoverSharing4' x = case x of
    SourceNode' -> return TrivialOpenGraph
    InnerNode' node -> do
        let z = unExprResult $ goSharing4 SourceNode node :: (TreeBuilder (OpenExpr e da) (OpenExpr e da dz))
        (final_node, graph) <- Sharing.runTreeBuilder z
        return (NontrivialOpenGraph final_node graph)
