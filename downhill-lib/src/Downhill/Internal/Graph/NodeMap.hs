{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}

module Downhill.Internal.Graph.NodeMap (
    NodeKey,
    NodeMap,
    SomeItem(..),
    map, mapWithKey,
    toList,
    zipWith,
    lookup, tryLookup,
    generate,
    IsNodeSet,
    fromList, List2,

    SomeNodeMap(..), fromOpenMap

) where
import Prelude(Maybe (Just, Nothing), error, (<$>), Foldable (foldr), const, (.))
import Downhill.Internal.Graph.OpenMap (OpenKey, OpenMap, SomeOpenItem(SomeOpenItem))
import qualified Downhill.Internal.Graph.OpenMap as OpenMap
import Data.Reflection (reify, Reifies(reflect))
import Data.Data (Proxy(Proxy))
import Data.Functor.Compose (Compose (Compose))

type List2 f = Compose [] f

type role NodeKey nominal nominal
newtype NodeKey s dx = NodeKey (OpenKey dx)

{- | Like 'OpenMap', but keeps track of nodes in types. Lookups never fail. Maps can
be zipped without losing any nodes.
-}
newtype NodeMap s f = NodeMap { unNodeMap :: OpenMap f }

data SomeItem s f = forall x. SomeItem (NodeKey s x) (f x)

class IsNodeSet s where
    nodesetDict :: OpenMap Proxy

map :: forall s f g. (forall v. f v -> g v) -> NodeMap s f -> NodeMap s g
map f = NodeMap . OpenMap.map f . unNodeMap

mapWithKey :: forall s f g. (forall dv. NodeKey s dv -> f dv -> g dv) -> NodeMap s f -> NodeMap s g
mapWithKey f (NodeMap x) = NodeMap (OpenMap.mapWithKey f' x)
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

tryLookup :: NodeMap s f -> OpenKey x -> Maybe (NodeKey s x, f x)
tryLookup (NodeMap m) key =
    case OpenMap.lookup m key of
        Just x -> Just (NodeKey key, x)
        Nothing -> Nothing

generate :: forall s f. IsNodeSet s => (forall x. NodeKey s x -> f x) -> NodeMap s f
generate f = case nodesetDict @s of
    m -> mapWithKey (\key _ -> f key) (NodeMap m)

zipWith :: forall s f g h. (forall x. f x -> g x -> h x) -> NodeMap s f -> NodeMap s g -> NodeMap s h
zipWith f (NodeMap x) (NodeMap y) = NodeMap (OpenMap.intersectionWith f x y)

adjust :: forall s f x. (f x -> f x) -> NodeKey s x -> NodeMap s f -> NodeMap s f
adjust f (NodeKey key) (NodeMap m) = NodeMap (OpenMap.adjust f key m)

fromList :: forall s f. IsNodeSet s => [SomeItem s f] -> NodeMap s (List2 f)
fromList = foldr prepend s0
    where prepend :: SomeItem s f -> NodeMap s (List2 f) -> NodeMap s (List2 f)
          prepend (SomeItem key value) = adjust (\(Compose xs) -> Compose (value:xs)) key
          s0 :: NodeMap s (List2 f)
          s0 = generate (const (Compose []))

data NodeSetWrapper s

instance Reifies s (OpenMap Proxy) => IsNodeSet (NodeSetWrapper s) where
    nodesetDict = reflect @s Proxy

data SomeNodeMap f where
    SomeNodeMap :: IsNodeSet s => NodeMap s f -> SomeNodeMap f

fromOpenMap :: forall f. OpenMap f -> SomeNodeMap f
fromOpenMap x = reify nodes go
    where nodes :: OpenMap Proxy
          nodes = OpenMap.map (const Proxy) x
          go :: forall s. Reifies s (OpenMap Proxy) => Proxy s -> SomeNodeMap f
          go _proxy = SomeNodeMap @(NodeSetWrapper s) (NodeMap x)
