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




import Expr(Expr5(Expr5))
import Sharing (BuildAction(BuildAction), TreeBuilder)
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))

type OpenArg = Endpoint OpenKey
type OpenTerm e = Edge OpenKey e
--type OpenExpr = Expr3 OpenKey
type OpenExpr e da = Node OpenKey e da


goSharing4 :: forall e da dv. Expr5 e da dv -> TreeBuilder (OpenExpr e da) (OpenExpr e da dv)
goSharing4 (Expr5 (Node xs)) = do
    let go' :: Edge (Expr5 e da) e da dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
        go' = goSharing4term
    xs' <- traverse go' xs
    return $ Node xs'

insertExpr3
  :: forall e da g dv.
     BuildAction (Expr5 e da) g
  -> Expr5 e da dv
  -> TreeBuilder g (OpenKey dv, g dv)
insertExpr3 x y@(Expr5 (Node _)) = do
    (k, z) <- Sharing.insertExpr x y
    return (k, z)

goSharing4arg :: forall e da dv. Endpoint (Expr5 e da) da dv -> TreeBuilder (OpenExpr e da) (OpenArg da dv)
goSharing4arg = \case
    SourceNode -> return SourceNode 
    InnerNode x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction4 x
        return (InnerNode xRef)

goSharing4term :: forall e da dv. Edge (Expr5 e da) e da dv -> TreeBuilder (OpenExpr e da) (OpenTerm e da dv)
goSharing4term = \case
    Edge f arg -> do
        arg' <- goSharing4arg arg
        return (Edge f arg')

sharingAction4 :: BuildAction (Expr5 e da) (OpenExpr e da)
sharingAction4 = BuildAction goSharing4

runRecoverSharing4 :: forall e da dz. Expr5 e da dz -> IO (OpenExpr e da dz, OpenMap (OpenExpr e da))
runRecoverSharing4 x = case x of
    Expr5 (Node _) -> do
      let z = goSharing4 x :: (TreeBuilder (OpenExpr e da) (OpenExpr e da dz))
      Sharing.runTreeBuilder z
