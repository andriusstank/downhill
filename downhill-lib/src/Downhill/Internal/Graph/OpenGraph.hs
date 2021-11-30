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

data OpenEdge e a v where
  OpenEdge :: e u v -> OpenEndpoint a u -> OpenEdge e a v

data OpenNode e a v = BasicVector v => OpenNode [OpenEdge e a v]

-- | Maintains a cache of visited 'Expr's.
newtype TreeBuilder e a r = TreeCache {unTreeCache :: StateT (OpenMap (OpenNode e a)) IO r}
  deriving (Functor, Applicative, Monad)

insertIntoCache :: OpenKey dv -> OpenNode e a dv -> TreeBuilder e a ()
insertIntoCache name value = TreeCache $ modify (OpenMap.insert name value)

-- | @buildExpr action key@ will run @action@, associate result with @key@ and
-- store it in cache. If @key@ is already in cache, @action@ will not be run.
buildExpr ::
  TreeBuilder e a (OpenNode e a v) ->
  Expr a v ->
  TreeBuilder e a (OpenKey v, OpenNode e a v)
buildExpr action key = do
  name <- TreeCache (lift (OpenMap.makeOpenKey key))
  cache <- TreeCache get
  case OpenMap.lookup cache name of
    Just x -> return (name, x)
    Nothing -> do
      value <- action
      insertIntoCache name value
      return (name, value)

runTreeBuilder :: forall e a g dv. TreeBuilder e a (g dv) -> IO (g dv, OpenMap (OpenNode e a))
runTreeBuilder rs_x = runStateT (unTreeCache rs_x) OpenMap.empty

-- | Computational graph under construction. "Open" refers to the set of the nodes – new nodes can be
-- added to this graph. Once the graph is complete the set of nodes will be frozen
-- and the type of the graph will become 'Graph' ("Downhill.Internal.Graph" module).
data OpenGraph e a z = OpenGraph (OpenNode e a z) (OpenMap (OpenNode e a))

goEdges :: BasicVector v => [Term a v] -> TreeBuilder BackFun a (OpenNode BackFun a v)
goEdges xs = do
  xs' <- traverse goSharing4term xs
  return $ OpenNode xs'

goSharing4arg :: forall a v. Expr a v -> TreeBuilder BackFun a (OpenEndpoint a v)
goSharing4arg key = case key of
  ExprVar -> return OpenSourceNode
  ExprSum xs -> do
    (gRef, _) <- buildExpr (goEdges xs) key
    return (OpenInnerNode gRef)

goSharing4term :: forall a v. Term a v -> TreeBuilder BackFun a (OpenEdge BackFun a v)
goSharing4term = \case
  Term f arg -> do
    arg' <- goSharing4arg arg
    return (OpenEdge (BackFun f) arg')

-- | Collects duplicate nodes in 'Expr' tree and converts it to a graph.
recoverSharing :: forall a z. BasicVector z => [Term a z] -> IO (OpenGraph BackFun a z)
recoverSharing xs = do
  (final_node, graph) <- runTreeBuilder (goEdges xs)
  return (OpenGraph final_node graph)
