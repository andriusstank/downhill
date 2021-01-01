{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module NodeMap where
import ExprRef
import Data.HashMap.Lazy (HashMap)
import GHC.StableName (StableName)
import GHC.Exts (Any)
import qualified ExprRef as ExprMap
import qualified Data.HashMap.Strict as Map
import Unsafe.Coerce (unsafeCoerce)
import Data.VectorSpace (AdditiveGroup)

data KeySet

newtype NodeKey (s :: KeySet) x dx = NodeKey (StableName Any)
newtype NodeMap (s :: KeySet) f = NodeMap { unNodeMap :: HashMap (StableName Any) (SomeExpr f) }

data SomeNodeMap f = forall s. SomeNodeMap (NodeMap s f)

data SomeItem (s :: KeySet) f = forall x dx. SomeItem (NodeKey s x dx) (f x dx)

data SomeValueWithNodeMap g f v dv = forall s. SomeValueWithNodeMap (g s v dv) (NodeMap s f)

{-# DEPRECATED unsafeFromExprMap, unsafeNodeKey "transitionary" #-}
unsafeFromExprMap :: ExprMap f -> NodeMap s f
unsafeFromExprMap (ExprMap x) = NodeMap x

unsafeNodeKey :: ExprName x dx -> NodeKey s x dx
unsafeNodeKey (ExprName x) = NodeKey x
toExprName :: NodeKey s x dx -> ExprName x dx
toExprName (NodeKey x) = ExprName x

{-# DEPRECATED toExprMap "transitionary" #-}
toExprMap :: NodeMap s f -> ExprMap f
toExprMap (NodeMap x) = ExprMap x

mapmap :: forall s f g. (forall v dv. f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmap f = unsafeFromExprMap . ExprMap.mapmap f . toExprMap

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
cvItem (SomeExprWithName (ExprName x) y) = SomeItem (NodeKey x) y

