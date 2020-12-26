{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module NodeMap where
import ExprRef
import Data.HashMap.Lazy (HashMap)
import GHC.StableName (StableName)
import GHC.Exts (Any)
import qualified ExprRef as ExprMap
import qualified Data.HashMap.Strict as Map
import Unsafe.Coerce (unsafeCoerce)

newtype NodeKey s x dx = NodeKey (StableName Any)
newtype NodeMap s f = NodeMap { unNodeMap :: HashMap (StableName Any) (SomeExpr f) }

data SomeNodeMap f = forall s. SomeNodeMap (NodeMap s f)

data SomeItem s f = forall x dx. SomeItem (NodeKey s x dx) (f x dx)

{-# DEPRECATED unsafeFromExprMap, unsafeNodeKey "transitionary" #-}
unsafeFromExprMap :: ExprMap f -> NodeMap s f
unsafeFromExprMap (ExprMap x) = NodeMap x

unsafeNodeKey :: ExprName x dx -> NodeKey s x dx
unsafeNodeKey (ExprName x) = NodeKey x

{-# DEPRECATED toExprMap "transitionary" #-}
toExprMap :: NodeMap s f -> ExprMap f
toExprMap (NodeMap x) = ExprMap x

mapmap :: forall s f g. (forall v dv. f v dv -> g v dv) -> NodeMap s f -> NodeMap s g
mapmap f = unsafeFromExprMap . ExprMap.mapmap f . toExprMap

toList :: NodeMap s f -> [SomeItem s f]
toList (NodeMap m) = wrap <$> Map.toList m
    where wrap :: (StableName Any, SomeExpr f) -> SomeItem s f
          wrap (xname, SomeExpr x) = SomeItem (NodeKey xname) x

--fromGraph :: ExprMap (ForwardInnerNode a da z dz) ForwardFinalNode a da z dz
-- data SomeItem f = forall x dx. SomeItem (ExprName x dx) (f x dx)

lookupExprName :: NodeMap s f -> ExprName v dv -> Maybe (f v dv)
lookupExprName (NodeMap m) (ExprName ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing

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
