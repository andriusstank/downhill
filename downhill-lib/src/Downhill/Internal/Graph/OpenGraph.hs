{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Downhill.Internal.Graph.OpenGraph (
    OpenGraph(..),
    OpenArg, OpenTerm, OpenExpr,
    recoverSharing
)
where
import Downhill.Linear.Expr(Expr(ExprSum, ExprVar), Term(..), BasicVector)
import Prelude hiding (lookup)
import Downhill.Internal.Graph.OpenMap (OpenMap, OpenKey)
import Downhill.Internal.Graph.Types (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge), BackFun (BackFun))
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
  -> Expr a v
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
-- added to this graph. Once the graph is complete the set of nodes will be frozen
-- and the type of the graph will become 'Graph' ("Downhill.Internal.Graph" module).
data OpenGraph e a z = OpenGraph (Node OpenKey e a z) (OpenMap (OpenExpr e a))

goEdges :: BasicVector v => [Term a v] -> TreeBuilder BackFun a (Node OpenKey BackFun a v)
goEdges xs = do
    xs' <- traverse goSharing4term xs
    return $ Node xs'

goSharing4arg :: forall a v. Expr a v -> TreeBuilder BackFun a (OpenArg a v)
goSharing4arg key = case key of
    ExprVar -> return SourceNode
    ExprSum xs -> do
        (gRef, _) <- buildExpr (goEdges xs) key
        return (InnerNode gRef)

goSharing4term :: forall a v. Term a v -> TreeBuilder BackFun a (OpenTerm BackFun a v)
goSharing4term = \case
    Term f arg -> do
        arg' <- goSharing4arg arg
        return (Edge (BackFun f) arg')

-- | Collects duplicate nodes in 'Expr' tree and converts it to a graph.
recoverSharing :: forall a z. BasicVector z => [Term a z] -> IO (OpenGraph BackFun a z)
recoverSharing xs = do
        (final_node, graph) <- runTreeBuilder (goEdges xs)
        return (OpenGraph final_node graph)

