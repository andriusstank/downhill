{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Downhill.Internal.Graph.NodeMap (
    NodeKey,
    NodeMap,
    SomeItem(..),
    mapmap, mapmapWithKey,
    toList,
    zipWith,
    lookup, tryLookup,
    generate,
    NodeSet,
    fromList, List2(..),

    SomeNodeMap(..), uncheckedMakeNodeMap

) where
import Prelude hiding (lookup, zipWith)
import Downhill.Internal.Graph.OpenMap (OpenKey, OpenMap, SomeOpenItem(SomeOpenItem))
import Downhill.Internal.Graph.OpenGraph (OpenExpr, OpenGraph (OpenGraph))
import qualified Downhill.Internal.Graph.OpenMap as OpenMap
import Data.Reflection (reify, Reifies(reflect))
import Data.Data (Proxy(Proxy))
import Downhill.Internal.Graph.Types (Node(Node), Endpoint (SourceNode, InnerNode), Edge(Edge))

data Unit dx = Unit

newtype List2 f dx = List2 [f dx]

type role NodeKey nominal nominal
newtype NodeKey s dx = NodeKey (OpenKey dx)
newtype NodeMap s f = NodeMap { unNodeMap :: OpenMap f }

data SomeItem s f = forall dx. SomeItem (NodeKey s dx) (f dx)

class NodeSet s where
    nodesetDict :: OpenMap Unit

mapmap :: forall s f g. (forall dv. f dv -> g dv) -> NodeMap s f -> NodeMap s g
mapmap f = NodeMap . OpenMap.mapmap f . unNodeMap

mapmapWithKey :: forall s f g. (forall dv. NodeKey s dv -> f dv -> g dv) -> NodeMap s f -> NodeMap s g
mapmapWithKey f (NodeMap x) = NodeMap (OpenMap.mapmapWithKey f' x)
    where f' :: OpenKey dx -> f dx -> g dx
          f' key' x' = f (NodeKey key') x'

toList :: NodeMap s f -> [SomeItem s f]
toList (NodeMap m) =  wrap <$> OpenMap.toList m
    where wrap :: SomeOpenItem f -> SomeItem s f
          wrap (SomeOpenItem key value) = SomeItem (NodeKey key) value

lookup :: NodeMap s f -> NodeKey s dv -> f dv
lookup (NodeMap m) (NodeKey key) =
    case OpenMap.lookup m key of
        Just x -> x
        Nothing -> error "oh fuck"

tryLookup :: NodeMap s f -> OpenKey dx -> Maybe (NodeKey s dx, f dx)
tryLookup (NodeMap m) key =
    case OpenMap.lookup m key of
        Just x -> Just (NodeKey key, x)
        Nothing -> Nothing

generate :: forall s f. NodeSet s => (forall dx. NodeKey s dx -> f dx) -> NodeMap s f
generate f = case nodesetDict @s of
    m -> mapmapWithKey (\key _ -> f key) (NodeMap m)

zipWith :: forall s f g h. (forall dx. f dx -> g dx -> h dx) -> NodeMap s f -> NodeMap s g -> NodeMap s h
zipWith f (NodeMap x) (NodeMap y) = NodeMap (OpenMap.intersectionWith f x y)

adjust :: forall s f dx. (f dx -> f dx) -> NodeKey s dx -> NodeMap s f -> NodeMap s f
adjust f (NodeKey key) (NodeMap m) = NodeMap (OpenMap.adjust f key m)

fromList :: forall s f. NodeSet s => [SomeItem s f] -> NodeMap s (List2 f)
fromList = foldr prepend s0
    where prepend :: SomeItem s f -> NodeMap s (List2 f) -> NodeMap s (List2 f)
          prepend (SomeItem key value) = adjust (\(List2 xs) -> List2 (value:xs)) key
          s0 :: NodeMap s (List2 f)
          s0 = generate (const (List2 []))

data NodeSetWrapper s

instance Reifies s (OpenMap Unit) => NodeSet (NodeSetWrapper s) where
    nodesetDict = reflect @s Proxy

data SomeNodeMap f where
    SomeNodeMap :: NodeSet s => NodeMap s f -> SomeNodeMap f

-- TODO: why "unchecked" in name?
uncheckedMakeNodeMap :: forall f. OpenMap f -> SomeNodeMap f
uncheckedMakeNodeMap x = reify nodes go
    where nodes :: OpenMap Unit
          nodes = OpenMap.mapmap (const Unit) x
          go :: forall s. Reifies s (OpenMap Unit) => Proxy s -> SomeNodeMap f
          go _proxy = SomeNodeMap @(NodeSetWrapper s) (NodeMap x)
