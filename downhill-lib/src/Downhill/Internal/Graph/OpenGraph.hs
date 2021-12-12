{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Downhill.Internal.Graph.OpenGraph
  ( OpenEdge (..),
    OpenEndpoint (..),
    OpenNode (..),
    OpenGraph (..),
    recoverSharing,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), get, modify)
import Downhill.Internal.Graph.OpenMap (OpenKey, OpenMap)
import qualified Downhill.Internal.Graph.OpenMap as OpenMap
import Downhill.Internal.Graph.Types (BackFun (BackFun))
import Downhill.Linear.Expr (BasicVector, Expr (ExprSum, ExprVar), Term (..))
import Prelude hiding (lookup)

data OpenEndpoint a v where
  OpenSourceNode :: OpenEndpoint a a
  OpenInnerNode :: OpenKey v -> OpenEndpoint a v

data OpenEdge a v where
  OpenEdge :: BackFun u v -> OpenEndpoint a u -> OpenEdge a v

data OpenNode a v = BasicVector v => OpenNode [OpenEdge a v]

-- | Maintains a cache of visited 'Expr's.
newtype TreeBuilder a r = TreeCache {unTreeCache :: StateT (OpenMap (OpenNode a)) IO r}
  deriving (Functor, Applicative, Monad)

insertIntoCache :: OpenKey dv -> OpenNode a dv -> TreeBuilder a ()
insertIntoCache name value = TreeCache $ modify (OpenMap.insert name value)

-- | @buildExpr action key@ will run @action@, associate result with @key@ and
-- store it in cache. If @key@ is already in cache, @action@ will not be run.
buildExpr ::
  TreeBuilder a (OpenNode a v) ->
  Expr a v ->
  TreeBuilder a (OpenKey v, OpenNode a v)
buildExpr action key = do
  name <- TreeCache (lift (OpenMap.makeOpenKey key))
  cache <- TreeCache get
  case OpenMap.lookup cache name of
    Just x -> return (name, x)
    Nothing -> do
      value <- action
      insertIntoCache name value
      return (name, value)

runTreeBuilder :: forall a g dv. TreeBuilder a (g dv) -> IO (g dv, OpenMap (OpenNode a))
runTreeBuilder rs_x = runStateT (unTreeCache rs_x) OpenMap.empty

-- | Computational graph under construction. "Open" refers to the set of the nodes – new nodes can be
-- added to this graph. Once the graph is complete the set of nodes will be frozen
-- and the type of the graph will become 'Graph' ("Downhill.Internal.Graph" module).
data OpenGraph a z = OpenGraph (OpenNode a z) (OpenMap (OpenNode a))

goEdges :: BasicVector v => [Term a v] -> TreeBuilder a (OpenNode a v)
goEdges xs = do
  xs' <- traverse goSharing4term xs
  return $ OpenNode xs'

goSharing4arg :: forall a v. Expr a v -> TreeBuilder a (OpenEndpoint a v)
goSharing4arg key = case key of
  ExprVar -> return OpenSourceNode
  ExprSum xs -> do
    (gRef, _) <- buildExpr (goEdges xs) key
    return (OpenInnerNode gRef)

goSharing4term :: forall a v. Term a v -> TreeBuilder a (OpenEdge a v)
goSharing4term = \case
  Term f arg -> do
    arg' <- goSharing4arg arg
    return (OpenEdge (BackFun f) arg')

-- | Collects duplicate nodes in 'Expr' tree and converts it to a graph.
recoverSharing :: forall a z. BasicVector z => [Term a z] -> IO (OpenGraph a z)
recoverSharing xs = do
  (final_node, graph) <- runTreeBuilder (goEdges xs)
  return (OpenGraph final_node graph)
