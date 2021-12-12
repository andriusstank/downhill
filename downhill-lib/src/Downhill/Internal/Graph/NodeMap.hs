{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Downhill.Internal.Graph.NodeMap
  ( -- * NodeMap
    NodeMap,
    NodeKey,
    -- * Construction
    fromOpenMap,
    generate,
    -- * Query
    lookup,
    tryLookup,
    toList,
    toListWith,
    elems,
    -- * Modify
    map,
    mapWithKey,
    adjust,
    zipWith,
    -- * Node Set
    IsNodeSet,
    SomeNodeMap (..),
    KeyAndValue (..),
  )
where

import Control.Applicative (Const)
import Data.Data (Proxy (Proxy))
import Data.Reflection (Reifies (reflect), reify)
import Downhill.Internal.Graph.OpenMap (OpenKey, OpenMap, SomeOpenItem (SomeOpenItem))
import qualified Downhill.Internal.Graph.OpenMap as OpenMap
import Prelude (Maybe (Just, Nothing), const, error, (.), (<$>))

type role NodeKey nominal nominal

-- | Valid key, guaranteed to be a member of @s@
newtype NodeKey s x = NodeKey (OpenKey x)

-- | @NodeMap s f@ is a map where value of type @f x@ is associated with key @NodeKey s x@.
-- Type variable `s` tracks the set of nodes. Lookups never fail. Maps can
-- be zipped without losing any nodes.
newtype NodeMap s f = NodeMap {unNodeMap :: OpenMap f}

data KeyAndValue s f = forall x. KeyAndValue (NodeKey s x) (f x)

class IsNodeSet s where
  allNodes :: OpenMap Proxy

map :: forall s f g. (forall v. f v -> g v) -> NodeMap s f -> NodeMap s g
map f = NodeMap . OpenMap.map f . unNodeMap

mapWithKey :: forall s f g. (forall x. NodeKey s x -> f x -> g x) -> NodeMap s f -> NodeMap s g
mapWithKey f (NodeMap x) = NodeMap (OpenMap.mapWithKey f' x)
  where
    f' :: OpenKey dx -> f dx -> g dx
    f' key' = f (NodeKey key')

toList :: NodeMap s f -> [KeyAndValue s f]
toList = toListWith KeyAndValue

toListWith :: forall s f r. (forall x. NodeKey s x -> f x -> r) -> NodeMap s f -> [r]
toListWith f (NodeMap m) = wrap <$> OpenMap.toList m
  where
    wrap :: SomeOpenItem f -> r
    wrap (SomeOpenItem key value) = f (NodeKey key) value

elems :: NodeMap s (Const b) -> [b]
elems (NodeMap m) = OpenMap.elems m

lookup :: NodeMap s f -> NodeKey s v -> f v
lookup (NodeMap m) (NodeKey key) =
  case OpenMap.lookup m key of
    Just x -> x
    Nothing -> error "oh fuck"

-- | If key belongs to @s@, @tryLookup@ will return a proof of this fact
-- and a corresponding value from the map. Otherwise returns @Nothing@.
tryLookup :: NodeMap s f -> OpenKey x -> Maybe (NodeKey s x, f x)
tryLookup (NodeMap m) key =
  case OpenMap.lookup m key of
    Just x -> Just (NodeKey key, x)
    Nothing -> Nothing

generate :: forall s f. IsNodeSet s => (forall x. NodeKey s x -> f x) -> NodeMap s f
generate f = case allNodes @s of
  m -> mapWithKey (\key _ -> f key) (NodeMap m)

zipWith :: forall s f g h. (forall x. f x -> g x -> h x) -> NodeMap s f -> NodeMap s g -> NodeMap s h
zipWith f (NodeMap x) (NodeMap y) = NodeMap (OpenMap.intersectionWith f x y)

adjust :: forall s f x. (f x -> f x) -> NodeKey s x -> NodeMap s f -> NodeMap s f
adjust f (NodeKey key) (NodeMap m) = NodeMap (OpenMap.adjust f key m)

data NodeSetWrapper s

instance Reifies s (OpenMap Proxy) => IsNodeSet (NodeSetWrapper s) where
  allNodes = reflect @s Proxy

-- | 'NodeMap' with existential set of nodes.
data SomeNodeMap f where
  SomeNodeMap :: IsNodeSet s => NodeMap s f -> SomeNodeMap f

fromOpenMap :: forall f. OpenMap f -> SomeNodeMap f
fromOpenMap x = reify nodes go
  where
    nodes :: OpenMap Proxy
    nodes = OpenMap.map (const Proxy) x
    go :: forall s. Reifies s (OpenMap Proxy) => Proxy s -> SomeNodeMap f
    go _proxy = SomeNodeMap @(NodeSetWrapper s) (NodeMap x)
