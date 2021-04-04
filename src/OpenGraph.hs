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
import Expr(Expr5(Expr5, Expr5Subs, Expr5Var), LinearFunc5, Edge'(..), Endpoint'(..))
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
    NoInsertExpr :: OpenArg a v -> ExprResult e a v
    DoInsertExpr :: TreeBuilder (OpenExpr e a) (OpenExpr e a v) -> ExprResult e a v

{-# DEPRECATED unExprResult, unInsertExprResult "remove" #-}
unExprResult :: ExprResult e a v -> TreeBuilder (OpenExpr e a) (OpenExpr e a v)
unExprResult = \case
    DoInsertExpr x -> x

data InsertExprResult e a v where
    NoInsertInsertExpr :: OpenArg a v -> InsertExprResult e a v
    DoInsertInsertExpr :: TreeBuilder (OpenExpr e a) (OpenKey v, OpenExpr e a v) -> InsertExprResult e a v

unInsertExprResult :: InsertExprResult e a v -> TreeBuilder (OpenExpr e a) (OpenKey v, OpenExpr e a v)
unInsertExprResult = \case
    DoInsertInsertExpr x -> x

insertExpr3 :: OpenArg a x -> Expr5 e x v -> InsertExprResult e a v
insertExpr3 x y = case goSharing4 x y of
    NoInsertExpr z -> NoInsertInsertExpr z
    DoInsertExpr z -> DoInsertInsertExpr $ do
                (k, zz) <- Sharing.insertExpr (BuildAction' z) y
                return (k, zz)

goSharing4 :: forall e x a v. OpenArg a x -> Expr5 e x v -> ExprResult e a v
goSharing4 src = \case
    Expr5Var -> NoInsertExpr src
    Expr5 xs -> DoInsertExpr $ do
        let go' :: Edge' e x v -> TreeBuilder (OpenExpr e a) (OpenTerm e a v)
            go' = goSharing4term src
        xs' <- traverse go' xs
        return $ Node xs'
    Expr5Subs f g -> DoInsertExpr $ do
        case f of
            SourceNode' -> unExprResult $ goSharing4 src g
            InnerNode' f' -> do
                (gRef, _sg) <- unInsertExprResult (insertExpr3 src g)
                unExprResult $ goSharing4 (InnerNode gRef) f'

goSharing4arg :: forall e dx da dv. OpenArg da dx -> Endpoint' e dx dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg src = \case
    SourceNode' -> return src
    InnerNode' g ->  do
        case insertExpr3 src g of
            NoInsertInsertExpr z -> return z
            DoInsertInsertExpr z -> do
                (gRef, _sg) <- z
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
