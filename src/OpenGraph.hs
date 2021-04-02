{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    runRecoverSharing4
)
where
import Expr(Expr5(Expr5, Expr5Subs), LinearFunc5(LinearFunc5))
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
    Expr5 (Node xs) -> do
        let go' :: Edge (Expr5 e dx) e dx dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
            go' = goSharing4term src
        xs' <- traverse go' xs
        return $ Node xs'
    Expr5Subs (LinearFunc5 f) g -> do
        case f of
            SourceNode -> goSharing4 src g
            InnerNode f' -> do
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

goSharing4arg :: forall e dx da dv. OpenArg da dx -> Endpoint (Expr5 e dx) dx dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg src = \case
    SourceNode -> return src
    InnerNode x ->  do
        (xRef, _sx) <- insertExpr3 (sharingAction4 src) x
        return (InnerNode xRef)

goSharing4term :: forall e dx da dv. OpenArg da dx -> Edge (Expr5 e dx) e dx dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
goSharing4term src = \case
    Edge f arg -> do
        arg' <- goSharing4arg src arg
        return (Edge f arg')

sharingAction4 :: OpenArg da dx -> BuildAction (Expr5 e dx) (OpenExpr e da)
sharingAction4 src = BuildAction (goSharing4 src)

runRecoverSharing4 :: forall e da dz. Expr5 e da dz -> IO (Endpoint (Node OpenKey e da) da dz, OpenMap (OpenExpr e da))
runRecoverSharing4 x = do
    let z = goSharing4 SourceNode x :: (TreeBuilder (OpenExpr e da) (OpenExpr e da dz))
    (x', m) <- Sharing.runTreeBuilder z
    return (InnerNode x', m)
