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
    unsafeCastNode,
    unsafeNodeKey,
    unsafeFromExprMap,
    toExprMap,
    NodeKey,
    SomeItem(..),
    toExprName,
    NodeMap,
    SharedArgS(..),
    SharedTermS(..),
    SharedExprS(..),
    mapmap,
    toList,
    fromListWith,
    mapmapWithKey,
    lookup,
    runRecoverSharing3,
    SharedExprWithMap,
    SomeNodeMap,
    SomeValueWithNodeMap,
    SomeSharedExprWithMap(..)
) where
import ExprRef
import Data.HashMap.Lazy (HashMap)
import GHC.StableName (StableName)
import GHC.Exts (Any)
import qualified ExprRef as ExprMap
import qualified Data.HashMap.Strict as Map
import Unsafe.Coerce (unsafeCoerce)
import Data.VectorSpace (AdditiveGroup)
import Tensor (AFunction)
import Sharing (TreeBuilder, ExprMap(..), SomeExpr(..), ExprName, SomeExprWithName(..), BuildAction(..))
import Expr (ExprArg(ArgExpr, ArgVar),  Term2(Func2),  Expr2(ExprSum))
import Prelude hiding (lookup)
import qualified Sharing

data KeySet

type role NodeKey nominal nominal nominal
newtype NodeKey (s :: KeySet) x dx = NodeKey (StableName Any)
newtype NodeMap (s :: KeySet) f = NodeMap { unNodeMap :: HashMap (StableName Any) (SomeExpr f) }

-- TODO: representable functor instance
data SomeNodeMap f = forall s. SomeNodeMap (NodeMap s f)

data SomeItem (s :: KeySet) f = forall x dx. SomeItem (NodeKey s x dx) (f x dx)

data SomeValueWithNodeMap g f v dv = forall s. SomeValueWithNodeMap (g s v dv) (NodeMap s f)

{-# DEPRECATED unsafeFromExprMap, unsafeNodeKey "transitionary" #-}
unsafeFromExprMap :: ExprMap f -> NodeMap s f
unsafeFromExprMap (ExprMap x) = NodeMap x

unsafeNodeKey :: ExprName -> NodeKey s x dx
unsafeNodeKey x = NodeKey x
toExprName :: NodeKey s x dx -> ExprName
toExprName (NodeKey x) = x

unsafeCastNode :: NodeKey s1 x dx -> NodeKey s2 x dx
unsafeCastNode (NodeKey x) = NodeKey x

{-# DEPRECATED toExprMap, unsafeCastNode "transitionary" #-}
toExprMap :: NodeMap s f -> ExprMap f
toExprMap (NodeMap x) = ExprMap x

mapmap :: forall s f g. (forall v dv. f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmap f = unsafeFromExprMap . Sharing.mapmap f . toExprMap

mapmapWithKey :: forall s f g. (forall v dv. NodeKey s v dv -> f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmapWithKey f (NodeMap x) = NodeMap (Map.mapWithKey go x)
    where go key (SomeExpr y) = SomeExpr (f (NodeKey key) y)


toList :: NodeMap s f -> [SomeItem s f]
toList (NodeMap m) = wrap <$> Map.toList m
    where wrap :: (StableName Any, SomeExpr f) -> SomeItem s f
          wrap (xname, SomeExpr x) = SomeItem (NodeKey xname) x

--fromGraph :: ExprMap (ForwardInnerNode a da z dz) ForwardFinalNode a da z dz
-- data SomeItem f = forall x dx. SomeItem (ExprName x dx) (f x dx)

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

cvItem :: SomeExprWithName f -> SomeItem s f
cvItem (SomeExprWithName x y) = SomeItem (NodeKey x) y



data SharedArgS s a da v dv where
    SharedArgVarS :: SharedArgS s a da a da
    SharedArgExprS :: NodeKey s v dv -> SharedArgS s a da v dv

data SharedTermS s a da v dv where
    SharedFunApS :: AFunction u du v dv -> SharedArgS s a da u du -> SharedTermS s a da v dv

data SharedExprS s a da v dv = (AdditiveGroup v, AdditiveGroup dv) => SharedExprSumS { unSharedExprSumS :: [SharedTermS s a da v dv] }

data SharedExprWithMap a da x dx = forall s. SharedExprWithMap (NodeMap s (SharedExprS s a da)) (SharedExprS s a da x dx)



goSharing3 :: forall s a da v dv. Expr2 a da v dv -> TreeBuilder (SharedExprS s a da) (SharedExprS s a da v dv)
goSharing3 (ExprSum xs) = do
    let go' :: Term2 a da v dv -> TreeBuilder (SharedExprS s a da) (SharedTermS s a da v dv)
        go' = goSharing3term
    xs' <- traverse go' xs
    return $ SharedExprSumS xs'

insertExpr3
  :: forall s a da g v dv.
     BuildAction (Expr2 a da) g
  -> Expr2 a da v dv
  -> TreeBuilder g (NodeKey s v dv, g v dv)
insertExpr3 x y@(ExprSum _) = do
    (ref, z) <- Sharing.insertExpr x y
    return (unsafeNodeKey ref, z)

goSharing3arg :: forall s a da v dv. ExprArg a da v dv -> TreeBuilder (SharedExprS s a da) (SharedArgS s a da v dv)
goSharing3arg = \case
    ArgVar -> return SharedArgVarS
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction3 x
        return (SharedArgExprS xRef)
goSharing3term :: forall s a da v dv. Term2 a da v dv -> TreeBuilder (SharedExprS s a da) (SharedTermS s a da v dv)
goSharing3term = \case
    Func2 f arg -> do
        arg' <- goSharing3arg arg
        return (SharedFunApS f arg')

sharingAction3 :: BuildAction (Expr2 a da) (SharedExprS s a da)
sharingAction3 = BuildAction goSharing3

data SomeSharedExprWithMap a da z dz where
    SomeSharedExprWithMap :: NodeMap s (SharedExprS s a da) -> SharedExprS s a da z dz -> SomeSharedExprWithMap a da z dz

runRecoverSharing3 :: forall a da v dv. Expr2 a da v dv -> IO (SomeSharedExprWithMap a da v dv)
runRecoverSharing3 x = case x of
    ExprSum _ -> do
      let z = goSharing3 x :: (TreeBuilder (SharedExprS s a da) (SharedExprS s a da v dv))
      (x', m) <- Sharing.runTreeBuilder z
      return (SomeSharedExprWithMap (unsafeFromExprMap m) x')
