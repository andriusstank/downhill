{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    OpenGraph(..),
    --runRecoverSharing4,
    --runRecoverSharing4',
    --runRecoverSharing6
)
where
import Expr(Expr5(Expr5, Expr5Subs, Expr5Var), LinearFunc5, Edge'(..), Endpoint'(..))
import Sharing (BuildAction(BuildAction), TreeBuilder, BuildAction'(..))
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))
import ExprWalker
import qualified OpenMap

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

{-# DEPRECATED unExprResult', unInsertExprResult "remove" #-}
unExprResult' :: ExprResult e a v -> TreeBuilder (OpenExpr e a) (OpenExpr e a v)
unExprResult' = \case
    DoInsertExpr x -> x

data InsertExprResult e a v where
    NoInsertInsertExpr :: OpenArg a v -> InsertExprResult e a v
    DoInsertInsertExpr :: TreeBuilder (OpenExpr e a) (OpenKey v) -> InsertExprResult e a v

unInsertExprResult :: InsertExprResult e a v -> TreeBuilder (OpenExpr e a) (OpenArg a v)
unInsertExprResult = \case
    NoInsertInsertExpr x -> case x of
        SourceNode -> return SourceNode
        InnerNode node -> return (InnerNode node)
    DoInsertInsertExpr x -> InnerNode <$> x

insertExpr3 :: OpenArg a x -> Expr5 e x v -> InsertExprResult e a v
insertExpr3 x y = case goSharing4 x y of
    NoInsertExpr z -> NoInsertInsertExpr z
    DoInsertExpr z -> DoInsertInsertExpr $ do
                (k, _zz) <- Sharing.insertExpr (BuildAction' z) y
                return k

insertExpr4 :: Expr5 e a v -> ExprResult e a v -> InsertExprResult e a v
insertExpr4 y w = case w of
    NoInsertExpr z -> NoInsertInsertExpr z
    DoInsertExpr z -> DoInsertInsertExpr $ do
                (k, _zz) <- Sharing.insertExpr (BuildAction' z) y
                return k


insertExpr5 :: OpenArg a x -> Expr5 e x v -> TreeBuilder (OpenExpr e a) (OpenArg a v)
insertExpr5 src x = unInsertExprResult (insertExpr3 src x)

{-
goSharing4_subs :: OpenArg a x1 -> Endpoint' e x2 v -> Endpoint' e x1 x2 -> ExprResult e a v
goSharing4_subs src f g = do
    f' <- goSharing4arg src f
    return (_ f')
-}

goSharing4 :: forall e x a v. OpenArg a x -> Expr5 e x v -> ExprResult e a v
goSharing4 src = \case
    Expr5Var -> NoInsertExpr src
    Expr5 xs -> DoInsertExpr $ do
        let go' :: Edge' e x v -> TreeBuilder (OpenExpr e a) (OpenTerm e a v)
            go' = goSharing4term src
        xs' <- traverse go' xs
        return $ Node xs'

{-
        case insertExpr3 src g of
                NoInsertInsertExpr z -> goSharing4 z f
                DoInsertInsertExpr z -> DoInsertExpr $ do
                    gRef <- z
                    unExprResult' $ goSharing4 (InnerNode gRef) f
-}
goSharing5 :: forall e x a v. OpenArg a x -> Expr5 e x v -> TreeBuilder (OpenExpr e a) (OpenArg a v)
goSharing5 src x = case goSharing4 src x of
    NoInsertExpr z -> return z
    DoInsertExpr z -> do
        (k, _zz) <- Sharing.insertExpr (BuildAction' z) x
        return (InnerNode k)

goSharing4arg :: forall e dx da dv. OpenArg da dx -> Endpoint' e dx dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg src = \case
    SourceNode' -> return src
    InnerNode' g ->  do
        case insertExpr3 src g of
            NoInsertInsertExpr z -> return z
            DoInsertInsertExpr z -> do
                gRef <- z
                return (InnerNode gRef)

goSharing4term :: forall e dx da dv. OpenArg da dx -> Edge' e dx dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
goSharing4term src = \case
    Edge' f arg -> do
        arg' <- goSharing4arg src arg
        return (Edge f arg')

data OpenGraph e a z where
    TrivialOpenGraph :: OpenGraph e a a
    NontrivialOpenGraph :: Node OpenKey e a z -> OpenMap (OpenExpr e a) -> OpenGraph e a z

--data TwoGraphs e a z = TwoGraphs (NodeKey a z) (OpenMap (CachedNode e a)) (OpenMap (OpenExpr e a))

runRecoverSharing4'' :: forall e da dz. LinearFunc5 e da dz -> IO (OpenGraph e da dz)
runRecoverSharing4'' x = case x of
    SourceNode' -> return TrivialOpenGraph
    InnerNode' node -> do
        let z = unExprResult' $ goSharing4 SourceNode node :: (TreeBuilder (OpenExpr e da) (OpenExpr e da dz))
        (final_node, graph) <- Sharing.runTreeBuilder z
        return (NontrivialOpenGraph final_node graph)

runRecoverSharing4' = runRecoverSharing5

cvtNode :: forall e a z. Node'' e a z -> Node OpenKey e a z
cvtNode (Node'' xs) = Node (cvtEdge <$> xs)
    where cvtEdge :: Edge'' e da dz -> Edge OpenKey e da dz
          cvtEdge = \case
            Edge'' f x -> case x of
                InnerKey x' -> Edge f (InnerNode x')

cvtCNode :: CachedNode e da dv -> Node OpenKey e da dv
cvtCNode (CachedInnerNode node) = cvtNode node

cvtGraph :: OpenMap (CachedNode e da) -> OpenMap (OpenExpr e da)
cvtGraph _ = OpenMap.empty -- OpenMap.mapmap cvtCNode

-- TODO: types
{-
reduceGraph :: forall e a. OpenMap (CachedNode e a) -> OpenMap (CachedNode e a)
reduceGraph xm = ym
    where ym = OpenMap.mapmapWithKey go xm
          go :: OpenKey v -> CachedNode e a v -> CachedNode e a
          go node = case node of
            CachedSourceNode -> SourceNode
            CachedClone (InnerKey src) -> case OpenMap.lookup ym src of
                Just y -> y
                Nothing -> error "oh fuck in reduceGraph"
            CachedInnerNode _ -> node
-}
runRecoverSharing5 :: forall e da dz. LinearFunc5 e da dz -> IO (OpenGraph e da dz)
runRecoverSharing5 x = case x of
    SourceNode' -> return TrivialOpenGraph
    InnerNode' node -> do
        let go = walkNode' node
        ((final_node_key, final_node_value), graph) <- Sharing.runTreeBuilder go
        case final_node_value of
            CachedSourceNode -> return TrivialOpenGraph
            CachedClone _ -> error "runRecoverSharing5: not implemented"
            CachedInnerNode node -> return (NontrivialOpenGraph (cvtNode node) (cvtGraph graph))

lftoexpr :: LinearFunc5 e a z -> Expr5 e a z
lftoexpr = \case
    SourceNode' -> Expr5Var 
    InnerNode' x -> x

runRecoverSharing6 :: forall e a z. LinearFunc5 e a z -> TreeBuilder (CachedNode e a) (OpenKey z, CachedNode e a z)
runRecoverSharing6 x = walkNode' (lftoexpr x)
