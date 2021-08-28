{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Downhill.Internal.Graph.OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    OpenGraph(..),
    recoverSharing
)
where
import Downhill.Linear.Expr(Expr(ExprSum, ExprVar), Term(..), BasicVector)
import Downhill.Internal.Graph.Sharing ()
import Prelude hiding (lookup)
import Downhill.Internal.Graph.OpenMap (OpenMap, OpenKey)
import Downhill.Internal.Graph.Types (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))
import Control.Monad.Trans.State.Strict ( StateT(..), get, modify )
import Control.Monad.Trans.Class(lift)
import qualified Downhill.Internal.Graph.OpenMap as OpenMap

-- | Maintains a cache of visited 'Expr's.
newtype TreeBuilder e a r = TreeCache { unTreeCache :: StateT (OpenMap (OpenExpr e a)) IO r }
    deriving (Functor, Applicative, Monad)

insertIntoCache :: OpenKey dv -> OpenExpr e a dv -> TreeBuilder e a ()
insertIntoCache name value = TreeCache $ modify (OpenMap.insert name value)

-- | @buildExpr action key@ will run @action@, associate result with @key@ and
-- store it in cache. If @key@ is already in cache, @action@ will not be run.
buildExpr
  :: TreeBuilder e a (OpenExpr e a v)
  -> Expr e a v
  -> TreeBuilder e a (OpenKey v, OpenExpr e a v)
buildExpr action key = do
    name <- TreeCache (lift (OpenMap.makeOpenKey key))
    cache <- TreeCache get
    case OpenMap.lookup cache name of
        Just x -> return (name, x)
        Nothing -> do
            value <- action
            insertIntoCache name value
            return (name, value)

runTreeBuilder :: forall e a g dv. TreeBuilder e a (g dv) -> IO (g dv, OpenMap (OpenExpr e a))
runTreeBuilder rs_x = runStateT (unTreeCache rs_x) OpenMap.empty



type OpenArg = Endpoint OpenKey
type OpenTerm e = Edge OpenKey e
type OpenExpr e da = Node OpenKey e da

-- | Computational graph under construction. "Open" refers to the set of the nodes – new nodes can be
-- added to this graph.
data OpenGraph e a z = OpenGraph (Node OpenKey e a z) (OpenMap (OpenExpr e a))

goEdges :: BasicVector v => [Term e a v] -> TreeBuilder e a (Node OpenKey e a v)
goEdges xs = do
    xs' <- traverse goSharing4term xs
    return $ Node xs'

goSharing4arg :: forall e a v. Expr e a v -> TreeBuilder e a (OpenArg a v)
goSharing4arg key = case key of
    ExprVar -> return SourceNode
    ExprSum xs -> do
        (gRef, _) <- buildExpr (goEdges xs) key
        return (InnerNode gRef)

goSharing4term :: forall e a v. Term e a v -> TreeBuilder e a (OpenTerm e a v)
goSharing4term = \case
    Term f arg -> do
        arg' <- goSharing4arg arg
        return (Edge f arg')

-- | Use observable sharing and ('System.Mem.StableName.StableName')
recoverSharing :: forall e a z. BasicVector z => [Term e a z] -> IO (OpenGraph e a z)
recoverSharing xs = do
        (final_node, graph) <- runTreeBuilder (goEdges xs)
        return (OpenGraph final_node graph)

