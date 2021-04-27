{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    OpenGraph(..),
    runRecoverSharing5,
    runRecoverSharing7
)
where
import Expr(Expr(ExprSum, ExprVar), Term(..), AnyExpr(..))
import Sharing (BuildAction(BuildAction), TreeBuilder, BuildAction'(..))
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))
import ExprWalker
import qualified OpenMap
import Notensor (BasicVector, FullVector (identityBuilder), BackFun(BackFun))

type OpenArg = Endpoint OpenKey
type OpenTerm e = Edge OpenKey e
type OpenExpr e da = Node OpenKey e da

data OpenGraph e a z where
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

runRecoverSharing5 :: forall e a z. BasicVector z => [Term e a z] -> IO (OpenGraph e a z)
runRecoverSharing5 xs = do
        (final_node, graph) <- Sharing.runTreeBuilder (goEdges xs)
        return (NontrivialOpenGraph final_node graph)

runRecoverSharing7 :: forall a z. FullVector z => AnyExpr BackFun a z -> IO (OpenGraph BackFun a z)
runRecoverSharing7 (AnyExpr x) = runRecoverSharing5 (x (BackFun identityBuilder))
