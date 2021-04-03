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
import Sharing (BuildAction(BuildAction), TreeBuilder)
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))

type OpenArg = Endpoint OpenKey
type OpenTerm e = Edge OpenKey e
type OpenExpr e da = Node OpenKey e da

goSharing4 :: forall e dx da dv. OpenArg da dx -> Expr5 e dx dv -> TreeBuilder (OpenExpr e da) (OpenExpr e da dv)
goSharing4 src = \case
    Expr5 xs -> do
        let go' :: Edge' e dx dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
            go' = goSharing4term src
        xs' <- traverse go' xs
        return $ Node xs'
    Expr5Subs f g -> do
        case f of
            SourceNode' -> goSharing4 src g
            InnerNode' f' -> do
                (gRef, _sg) <- insertExpr3 (sharingAction4 src) g
                goSharing4 (InnerNode gRef) f'

insertExpr3
  :: forall e dx g dv.
     BuildAction (Expr5 e dx) g
  -> Expr5 e dx dv
  -> TreeBuilder g (OpenKey dv, g dv)
insertExpr3 x y = do
    (k, z) <- Sharing.insertExpr x y
    return (k, z)

goSharing4arg :: forall e dx da dv. OpenArg da dx -> Endpoint' e dx dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg src = \case
    SourceNode' -> return src
    InnerNode' x ->  do
        (xRef, _sx) <- insertExpr3 (sharingAction4 src) x
        return (InnerNode xRef)

goSharing4term :: forall e dx da dv. OpenArg da dx -> Edge' e dx dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
goSharing4term src = \case
    Edge' f arg -> do
        arg' <- goSharing4arg src arg
        return (Edge f arg')

sharingAction4 :: OpenArg da dx -> BuildAction (Expr5 e dx) (OpenExpr e da)
sharingAction4 src = BuildAction (goSharing4 src)

data OpenGraph e a z where
    TrivialOpenGraph :: OpenGraph e a a
    NontrivialOpenGraph :: Node OpenKey e a z -> OpenMap (OpenExpr e a) -> OpenGraph e a z

runRecoverSharing4' :: forall e da dz. LinearFunc5 e da dz -> IO (OpenGraph e da dz)
runRecoverSharing4' x = case x of
    SourceNode' -> return TrivialOpenGraph
    InnerNode' node -> do
        let z = goSharing4 SourceNode node :: (TreeBuilder (OpenExpr e da) (OpenExpr e da dz))
        (final_node, graph) <- Sharing.runTreeBuilder z
        return (NontrivialOpenGraph final_node graph)
