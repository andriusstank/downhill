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
{-# OPTIONS_GHC -Wno-unused-imports #-}
module NodeMap (
    NodeKey,
    SomeItem(..),
    NodeMap,
    SharedArgS,
    SharedTermS,
    SharedExprS,
    mapmap, mapmapWithKey,
    toList,
    zipWith,
    lookup,
    generate,
    runRecoverSharing5,
    SomeSharedExprWithMap(..),
    uncheckedMakeNodeMap,
    NodeSet,

    fromList, List2(..)
) where
import Data.HashMap.Lazy (HashMap)
import GHC.StableName (StableName)
import GHC.Exts (Any)
import qualified Data.HashMap.Strict as Map
import Unsafe.Coerce (unsafeCoerce)
import Data.VectorSpace (AdditiveGroup)
import Tensor (AFunction)
import Sharing (TreeBuilder, SomeExpr(..), BuildAction(..))
import Expr (ExprArg(ArgExpr, ArgVar), Term3(Func2),  Expr3(ExprSum), Expr2(Expr2))
import Prelude hiding (lookup, zipWith)
import OpenMap (OpenKey, OpenMap, SomeOpenItem(SomeOpenItem))
import OpenGraph (OpenExpr)
import qualified OpenGraph
import qualified Sharing
import qualified OpenMap
import Data.Reflection (reify, Reifies(reflect))
import Data.Data (Proxy(Proxy))
import Data.Constraint (Dict(Dict))
import qualified Data.HashMap.Lazy as HashMap

data Unit x dx = Unit

newtype List2 f x dx = List2 [f x dx]

type role NodeKey nominal nominal nominal
newtype NodeKey s x dx = NodeKey (OpenKey x dx)
newtype NodeMap s f = NodeMap { unNodeMap :: OpenMap f }

data SomeItem s f = forall x dx. SomeItem (NodeKey s x dx) (f x dx)

class NodeSet s where
    nodesetDict :: OpenMap Unit

mapmap :: forall s f g. (forall v dv. f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmap f = NodeMap . OpenMap.mapmap f . unNodeMap

mapmapWithKey :: forall s f g. (forall v dv. NodeKey s v dv -> f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmapWithKey f (NodeMap x) = NodeMap (OpenMap.mapmapWithKey f' x)
    where f' :: OpenKey x dx -> f x dx -> g x dx
          f' key' x' = f (NodeKey key') x'

toList :: NodeMap s f -> [SomeItem s f]
toList (NodeMap m) =  wrap <$> OpenMap.toList m
    where wrap :: SomeOpenItem f -> SomeItem s f
          wrap (SomeOpenItem key value) = SomeItem (NodeKey key) value

lookup :: NodeMap s f -> NodeKey s v dv -> f v dv
lookup (NodeMap m) (NodeKey key) =
    case OpenMap.lookup m key of
        Just x -> x
        Nothing -> error "oh fuck"

tryLookup :: NodeMap s f -> OpenKey x dx -> Maybe (NodeKey s x dx, f x dx)
tryLookup (NodeMap m) key =
    case OpenMap.lookup m key of
        Just x -> Just (NodeKey key, x)
        Nothing -> Nothing

generate :: forall s f. NodeSet s => (forall x dx. NodeKey s x dx -> f x dx) -> NodeMap s f
generate f = case nodesetDict @s of
    m -> mapmapWithKey (\key _ -> f key) (NodeMap m)

zipWith :: forall s f g h. (forall x dx. f x dx -> g x dx -> h x dx) -> NodeMap s f -> NodeMap s g -> NodeMap s h
zipWith f (NodeMap x) (NodeMap y) = NodeMap (OpenMap.intersectionWith f x y)

adjust :: forall s f x dx. (f x dx -> f x dx) -> NodeKey s x dx -> NodeMap s f -> NodeMap s f
adjust f (NodeKey key) (NodeMap m) = NodeMap (OpenMap.adjust f key m)

fromList :: forall s f. NodeSet s => [SomeItem s f] -> NodeMap s (List2 f)
fromList = foldr prepend s0
    where prepend :: SomeItem s f -> NodeMap s (List2 f) -> NodeMap s (List2 f)
          prepend (SomeItem key value) = adjust (\(List2 xs) -> List2 (value:xs)) key
          s0 :: NodeMap s (List2 f)
          s0 = generate (const (List2 []))

type SharedArgS s = ExprArg (NodeKey s)
type SharedTermS s = Term3 (NodeKey s)
type SharedExprS s = Expr3 (NodeKey s)

data SomeSharedExprWithMap a da z dz where
    SomeSharedExprWithMap :: NodeSet s => NodeMap s (SharedExprS s a da) -> SharedExprS s a da z dz -> SomeSharedExprWithMap a da z dz

cvthelper :: forall s a da v dv. NodeSet s => NodeMap s (OpenExpr a da) -> OpenExpr a da v dv -> SomeSharedExprWithMap a da v dv
cvthelper m x = SomeSharedExprWithMap (mapmap cvtexpr m) (cvtexpr x)
    where cvtexpr :: forall x dx. OpenExpr a da x dx -> SharedExprS s a da x dx
          cvtexpr = \case
            ExprSum terms -> ExprSum (cvtterm <$> terms)
          cvtterm :: forall x dx. Term3 OpenKey a da x dx -> Term3 (NodeKey s) a da x dx
          cvtterm = \case
            Func2 f x' -> Func2 f (cvtarg x')
          cvtarg :: forall u du. ExprArg OpenKey a da u du -> ExprArg (NodeKey s) a da u du
          cvtarg = \case
            ArgVar -> ArgVar
            ArgExpr key -> case tryLookup m key of
                Just (key', _value) -> ArgExpr key'
                Nothing -> error "oh fuck"
          

cvtmap :: (OpenExpr a da v dv, OpenMap (OpenExpr a da)) -> SomeSharedExprWithMap a da v dv
cvtmap (x, m) = case uncheckedMakeNodeMap m of
    SomeNodeMap m' -> cvthelper m' x

runRecoverSharing5 :: forall a da v dv. Expr2 a da v dv -> IO (SomeSharedExprWithMap a da v dv)
runRecoverSharing5 x = cvtmap <$> OpenGraph.runRecoverSharing4 x

data SomeNodeMap f where
    SomeNodeMap :: NodeSet s => NodeMap s f -> SomeNodeMap f

data NodeSetWrapper s

instance Reifies s (OpenMap Unit) => NodeSet (NodeSetWrapper s) where
    nodesetDict = reflect @s Proxy

uncheckedMakeNodeMap :: forall f. OpenMap f -> SomeNodeMap f
uncheckedMakeNodeMap x = reify nodes go
    where nodes :: OpenMap Unit
          nodes = OpenMap.mapmap (const Unit) x
          go :: forall s. Reifies s (OpenMap Unit) => Proxy s -> SomeNodeMap f
          go _proxy = SomeNodeMap @(NodeSetWrapper s) (NodeMap x)

