{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module NodeMap where
import ExprRef
import ExprRef
import Data.HashMap.Lazy (HashMap)
import GHC.StableName (StableName)
import GHC.Exts (Any)

newtype NodeKey s x dx = NodeKey (StableName Any)
newtype NodeMap s f = NodeMap { unNodeMap :: HashMap (StableName Any) (SomeExpr f) }

data SomeNodeMap f = forall s. SomeNodeMap (NodeMap s f)

{- DEPRECATED unsafeFromExprMap, unsafeNodeKey "transitionary" -}
unsafeFromExprMap :: ExprMap f -> NodeMap s f
unsafeFromExprMap (ExprMap x) = NodeMap x

unsafeNodeKey :: ExprName x dx -> NodeKey s x dx
unsafeNodeKey (ExprName x) = NodeKey x

{-# DEPRECATED unsafeFromExprMap, unsafeNodeKey "transitionary" #-}
toExprMap :: NodeMap s f -> ExprMap f
toExprMap (NodeMap x) = ExprMap x

--fromGraph :: ExprMap (ForwardInnerNode a da z dz) ForwardFinalNode a da z dz
-- data SomeItem f = forall x dx. SomeItem (ExprName x dx) (f x dx)
