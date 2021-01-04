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
    mapmap,
    toList,
    fromListWith,
    mapmapWithKey,
    lookup,
    runRecoverSharing3,
    SomeSharedExprWithMap(..)
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
import OpenGraph
import qualified Sharing

data KeySet

type role NodeKey nominal nominal nominal
newtype NodeKey (s :: KeySet) x dx = NodeKey (StableName Any)
newtype NodeMap (s :: KeySet) f = NodeMap { unNodeMap :: HashMap (StableName Any) (SomeExpr f) }

-- TODO: representable functor instance

data SomeItem (s :: KeySet) f = forall x dx. SomeItem (NodeKey s x dx) (f x dx)

mapmap :: forall s f g. (forall v dv. f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmap f = NodeMap . Sharing.mapmap f . unNodeMap

mapmapWithKey :: forall s f g. (forall v dv. NodeKey s v dv -> f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmapWithKey f (NodeMap x) = NodeMap (Map.mapWithKey go x)
    where go key (SomeExpr y) = SomeExpr (f (NodeKey key) y)


toList :: NodeMap s f -> [SomeItem s f]
toList (NodeMap m) = wrap <$> Map.toList m
    where wrap :: (StableName Any, SomeExpr f) -> SomeItem s f
          wrap (xname, SomeExpr x) = SomeItem (NodeKey xname) x

unsafeCastType''' :: SomeExpr f -> f v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookup :: NodeMap s f -> NodeKey s v dv -> f v dv
lookup (NodeMap m) (NodeKey ref) =
    case Map.lookup ref m of
        Just x -> unsafeCastType''' x
        Nothing -> error "oh fuck"

fromListWith :: forall s f. [SomeItem s f] -> (forall x dx. f x dx -> f x dx -> f x dx) -> NodeMap s f
fromListWith xs f = NodeMap (Map.fromListWith f' (go <$> xs))
    where go :: SomeItem s f -> (StableName Any, SomeExpr f)
          go (SomeItem (NodeKey xname) x) = (xname, SomeExpr x)
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
    return (NodeKey ref, z)

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
      return (SomeSharedExprWithMap (NodeMap m) x')

--fromOpenGraph :: OpenExprWithMap a da z dz -> SomeSharedExprWithMap a da z dz
--fromOpenGraph = _