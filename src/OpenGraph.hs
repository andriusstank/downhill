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
import Notensor (AFunction1)

type OpenArg = Endpoint OpenKey
type OpenTerm = Edge OpenKey AFunction1
--type OpenExpr = Expr3 OpenKey
type OpenExpr da = Node OpenKey AFunction1 da


goSharing4 :: forall da dv. Expr5 da dv -> TreeBuilder (OpenExpr da) (OpenExpr da dv)
goSharing4 (Expr5 (Node xs)) = do
    let go' :: Edge (Expr5 da) AFunction1 da dv -> TreeBuilder (OpenExpr da) (OpenTerm da dv)
        go' = goSharing4term
    xs' <- traverse go' xs
    return $ Node xs'

insertExpr3
  :: forall da g dv.
     BuildAction (Expr5 da) g
  -> Expr5 da dv
  -> TreeBuilder g (OpenKey dv, g dv)
insertExpr3 x y@(Expr5 (Node _)) = do
    (k, z) <- Sharing.insertExpr x y
    return (k, z)

goSharing4arg :: forall da dv. Endpoint (Expr5 da) da dv -> TreeBuilder (OpenExpr da) (OpenArg da dv)
goSharing4arg = \case
    SourceNode -> return SourceNode 
    InnerNode x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction4 x
        return (InnerNode xRef)

goSharing4term :: forall da dv. Edge (Expr5 da) AFunction1 da dv -> TreeBuilder (OpenExpr da) (OpenTerm da dv)
goSharing4term = \case
    Edge f arg -> do
        arg' <- goSharing4arg arg
        return (Edge f arg')

sharingAction4 :: BuildAction (Expr5 da) (OpenExpr da)
sharingAction4 = BuildAction goSharing4

runRecoverSharing4 :: forall da dz. Expr5 da dz -> IO (OpenExpr da dz, OpenMap (OpenExpr da))
runRecoverSharing4 x = case x of
    Expr5 (Node _) -> do
      let z = goSharing4 x :: (TreeBuilder (OpenExpr da) (OpenExpr da dz))
      Sharing.runTreeBuilder z
