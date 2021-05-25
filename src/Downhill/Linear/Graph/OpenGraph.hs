{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

module Downhill.Linear.Graph.OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    OpenGraph(..),
    recoverSharing
)
where
import Downhill.Linear.Expr(Expr(ExprSum, ExprVar), Term(..), BasicVector)
import Sharing (BuildAction(BuildAction), TreeBuilder, BuildAction'(..))
import qualified Sharing
import Prelude hiding (lookup)
import Downhill.Linear.Graph.OpenMap (OpenMap, OpenKey)
import Downhill.Linear.Graph.Types (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))
import ExprWalker
import qualified Downhill.Linear.Graph.OpenMap as OpenMap
import Notensor (FullVector (identityBuilder))

type OpenArg = Endpoint OpenKey
type OpenTerm e = Edge OpenKey e
type OpenExpr e da = Node OpenKey e da

-- | Computational graph under construction. "Open" refers to the set of the nodes – new nodes can be
-- added to this graph.
data OpenGraph e a z = OpenGraph (Node OpenKey e a z) (OpenMap (OpenExpr e a))

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


recoverSharing :: forall e a z. BasicVector z => [Term e a z] -> IO (OpenGraph e a z)
recoverSharing xs = do
        (final_node, graph) <- Sharing.runTreeBuilder (goEdges xs)
        return (OpenGraph final_node graph)

