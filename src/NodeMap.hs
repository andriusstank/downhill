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
    fromListWith,
    lookup,
    runRecoverSharing3,
    SomeSharedExprWithMap(..),
    uncheckedMakeNodeMap,
) where
import Data.HashMap.Lazy (HashMap)
import GHC.StableName (StableName)
import GHC.Exts (Any)
import qualified Data.HashMap.Strict as Map
import Unsafe.Coerce (unsafeCoerce)
import Data.VectorSpace (AdditiveGroup)
import Tensor (AFunction)
import Sharing (TreeBuilder, ExprMap, SomeExpr(..), ExprName, SomeExprWithName(..), BuildAction(..))
import Expr (ExprArg(ArgExpr, ArgVar), Term3(Func2),  Expr3(ExprSum), Expr2(Expr2))
import Prelude hiding (lookup)
import OpenGraph (OpenExpr, OpenExprWithMap(OpenExprWithMap), OpenKey(OpenKey), OpenMap(OpenMap), SomeOpenItem(SomeOpenItem))
import qualified OpenGraph
import qualified Sharing
import Data.Reflection (reify, Reifies(reflect))
import Data.Data (Proxy(Proxy))
import Data.Constraint (Dict(Dict))

data Unit x dx = Unit

type role NodeKey nominal nominal nominal
newtype NodeKey s x dx = NodeKey (OpenKey x dx)
newtype NodeMap s f = NodeMap { unNodeMap :: OpenMap f }

-- TODO: representable functor instance

data SomeItem s f = forall x dx. SomeItem (NodeKey s x dx) (f x dx)

mapmap :: forall s f g. (forall v dv. f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmap f = NodeMap . OpenGraph.mapmap f . unNodeMap

mapmapWithKey :: forall s f g. (forall v dv. NodeKey s v dv -> f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmapWithKey f (NodeMap x) = NodeMap (OpenGraph.mapmapWithKey f' x)
    where f' :: OpenKey x dx -> f x dx -> g x dx
          f' key' x' = f (NodeKey key') x'

toList :: NodeMap s f -> [SomeItem s f]
toList (NodeMap m) =  wrap<$> OpenGraph.toList m
    where wrap :: SomeOpenItem f -> SomeItem s f
          wrap (SomeOpenItem key value) = SomeItem (NodeKey key) value

unsafeCastType''' :: SomeExpr f -> f v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookup :: NodeMap s f -> NodeKey s v dv -> f v dv
lookup (NodeMap m) (NodeKey key) =
    case OpenGraph.lookup m key of
        Just x -> x
        Nothing -> error "oh fuck"

tryLookup :: NodeMap s f -> OpenKey x dx -> Maybe (NodeKey s x dx, f x dx)
tryLookup (NodeMap m) key =
    case OpenGraph.lookup m key of
        Just x -> Just (NodeKey key, x)
        Nothing -> Nothing

-- TODO: use `s` to create map
fromListWith :: forall s f. [SomeItem s f] -> (forall x dx. f x dx -> f x dx -> f x dx) -> NodeMap s f
fromListWith xs f = NodeMap (OpenMap (Map.fromListWith f' (go <$> xs)))
    where go :: SomeItem s f -> (StableName Any, SomeExpr f)
          go (SomeItem (NodeKey (OpenKey xname)) x) = (xname, SomeExpr x)
          f' (SomeExpr x) (SomeExpr y) = SomeExpr (f x (unsafeCoerce y))

type SharedArgS s = ExprArg (NodeKey s)
type SharedTermS s = Term3 (NodeKey s)
type SharedExprS s = Expr3 (NodeKey s)

{-
data SharedArgS s a da v dv where
    SharedArgVarS :: SharedArgS s a da a da
    SharedArgExprS :: NodeKey s v dv -> SharedArgS s a da v dv

data SharedTermS s a da v dv where
    SharedFunApS :: AFunction u du v dv -> SharedArgS s a da u du -> SharedTermS s a da v dv

data SharedExprS s a da v dv = (AdditiveGroup v, AdditiveGroup dv) => SharedExprSumS { unSharedExprSumS :: [SharedTermS s a da v dv] }
-}

goSharing3 :: forall s a da v dv. Expr2 a da v dv -> TreeBuilder (SharedExprS s a da) (SharedExprS s a da v dv)
goSharing3 (Expr2 (ExprSum xs)) = do
    let go' :: Term3 (Expr2 a da) a da v dv -> TreeBuilder (SharedExprS s a da) (SharedTermS s a da v dv)
        go' = goSharing3term
    xs' <- traverse go' xs
    return $ ExprSum xs'

insertExpr3
  :: forall s a da g v dv.
     BuildAction (Expr2 a da) g
  -> Expr2 a da v dv
  -> TreeBuilder g (NodeKey s v dv, g v dv)
insertExpr3 x y@(Expr2 (ExprSum _)) = do
    (ref, z) <- Sharing.insertExpr x y
    return (NodeKey (OpenKey ref), z)

goSharing3arg :: forall s a da v dv. ExprArg (Expr2 a da) a da v dv -> TreeBuilder (SharedExprS s a da) (SharedArgS s a da v dv)
goSharing3arg = \case
    ArgVar -> return ArgVar
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction3 x
        return (ArgExpr xRef)
goSharing3term :: forall s a da v dv. Term3 (Expr2 a da) a da v dv -> TreeBuilder (SharedExprS s a da) (SharedTermS s a da v dv)
goSharing3term = \case
    Func2 f arg -> do
        arg' <- goSharing3arg arg
        return (Func2 f arg')

sharingAction3 :: BuildAction (Expr2 a da) (SharedExprS s a da)
sharingAction3 = BuildAction goSharing3

data SomeSharedExprWithMap a da z dz where
    SomeSharedExprWithMap :: NodeMap s (SharedExprS s a da) -> SharedExprS s a da z dz -> SomeSharedExprWithMap a da z dz

runRecoverSharing3 :: forall a da v dv. Expr2 a da v dv -> IO (SomeSharedExprWithMap a da v dv)
runRecoverSharing3 x = case x of
    Expr2 (ExprSum _) -> do
      let z = goSharing3 x :: (TreeBuilder (SharedExprS s a da) (SharedExprS s a da v dv))
      (x', m) <- Sharing.runTreeBuilder z
      return (SomeSharedExprWithMap (NodeMap (OpenMap m)) x')

cvthelper :: forall s a da v dv. NodeMap s (OpenExpr a da) -> OpenExpr a da v dv -> SomeSharedExprWithMap a da v dv
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
                Just (key, _value) -> ArgExpr key
                Nothing -> error "oh fuck"
          

cvtmap :: OpenExprWithMap a da v dv -> SomeSharedExprWithMap a da v dv
cvtmap (OpenExprWithMap m x) = case uncheckedMakeNodeMap m of
    SomeNodeMap m' -> cvthelper m' x

runRecoverSharing5' :: forall a da v dv. Expr2 a da v dv -> IO (SomeSharedExprWithMap a da v dv)
runRecoverSharing5' x = cvtmap <$> OpenGraph.runRecoverSharing4 x

data SomeNodeMap f where
    SomeNodeMap :: Reifies s (OpenMap Unit) => NodeMap s f -> SomeNodeMap f

uncheckedMakeNodeMap :: forall f. OpenMap f -> SomeNodeMap f
uncheckedMakeNodeMap x = reify nodes go
    where nodes :: OpenMap Unit
          nodes = OpenGraph.mapmap (const Unit) x
          go :: forall s. Reifies s (OpenMap Unit) => Proxy s -> SomeNodeMap f
          go _proxy = SomeNodeMap @s (NodeMap x)

