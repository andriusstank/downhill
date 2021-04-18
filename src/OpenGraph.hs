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
import Expr(Expr(ExprSum, ExprVar), Term(..))
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

data OpenGraph e a z where
    TrivialOpenGraph :: OpenGraph e a a
    NontrivialOpenGraph :: Node OpenKey e a z -> OpenMap (OpenExpr e a) -> OpenGraph e a z

goEdges :: BasicVector v => [Term e a v] -> TreeBuilder (OpenExpr e a) (Node OpenKey e a v)
goEdges xs = do
    xs' <- traverse goSharing4term xs
    return $ Node xs'

goSharing4arg :: forall e a v. Expr e a v -> TreeBuilder (OpenExpr e a) (OpenArg a v)
goSharing4arg key = case key of
    ExprVar -> return SourceNode
    ExprSum xs -> do
        (gRef, _) <- Sharing.insertExpr (goEdges xs) key
        return (InnerNode gRef)

goSharing4term :: forall e a v. Term e a v -> TreeBuilder (OpenExpr e a) (OpenTerm e a v)
goSharing4term = \case
    Term f arg -> do
        arg' <- goSharing4arg arg
        return (Edge f arg')

runRecoverSharing4 :: forall e a z. Expr e a z -> IO (OpenGraph e a z)
runRecoverSharing4 = \case
    ExprVar -> return TrivialOpenGraph
    ExprSum xs -> do
        (final_node, graph) <- Sharing.runTreeBuilder (goEdges xs)
        return (NontrivialOpenGraph final_node graph)
