{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
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
import Tensor (AFunction)
import Sharing (goSharing2, SharedArg(SharedArgExpr, SharedArgVar),  SharedTerm(SharedFunAp),  SharedExpr(SharedExprSum))
import Expr (ExprArg(ArgExpr, ArgVar),  Term2(Func2),  Expr2(ExprSum))

data KeySet

type role NodeKey nominal nominal nominal
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

unsafeCastNode :: NodeKey s1 x dx -> NodeKey s2 x dx
unsafeCastNode (NodeKey x) = NodeKey x

{-# DEPRECATED toExprMap, unsafeCastNode "transitionary" #-}
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



data SharedArgS s a da v dv where
    SharedArgVarS :: SharedArgS s a da a da
    SharedArgExprS :: NodeKey s v dv -> SharedArgS s a da v dv

data SharedTermS s a da v dv where
    SharedFunApS :: AFunction u du v dv -> SharedArgS s a da u du -> SharedTermS s a da v dv

data SharedExprS s a da v dv = (AdditiveGroup v, AdditiveGroup dv) => SharedExprSumS { unSharedExprSumS :: [SharedTermS s a da v dv] }

toS :: SharedExpr a da v dv -> SharedExprS s a da v dv
toS = \case
    SharedExprSum xs -> SharedExprSumS (termToS <$> xs)

termToS :: SharedTerm a da v dv -> SharedTermS s a da v dv
termToS (SharedFunAp f x)  = SharedFunApS f (argToS x)

argToS :: SharedArg a da v dv -> SharedArgS s a da v dv
argToS = \case
    SharedArgVar -> SharedArgVarS
    SharedArgExpr x -> SharedArgExprS (unsafeNodeKey x)

forgetSharing2 :: forall s a v da dv. (SharedExprS s a da v dv, NodeMap s (SharedExprS s a da)) -> Expr2 a da v dv
forgetSharing2 (x, env) = goSum x
    where goSum :: forall x dx. SharedExprS s a da x dx -> Expr2 a da x dx
          goSum = \case
            SharedExprSumS xs -> ExprSum (goTerm <$> xs)
          goTerm :: forall x dx. SharedTermS s a da x dx -> Term2 a da x dx
          goTerm = \case
            SharedFunApS f arg -> Func2 f (goArg arg)
          goArg :: forall x dx. SharedArgS s a da x dx -> ExprArg a da x dx
          goArg = \case
            SharedArgVarS -> ArgVar
            SharedArgExprS xname -> case NodeMap.lookup env (NodeMap.unsafeCastNode xname) of
                y -> ArgExpr (goSum y)

data SharedExprWithMap a da x dx = forall s. SharedExprWithMap (NodeMap s (SharedExprS s a da)) (SharedExprS s a da x dx)



runRecoverSharing2 :: forall a da v dv. Expr2 a da v dv -> IO (SharedExprWithMap a da v dv)
runRecoverSharing2 x = case x of
    ExprSum _ -> do
      let z = goSharing2 x :: (TreeBuilder (SharedExpr a da) (SharedExpr a da v dv))
          z' =  toS <$> z :: TreeBuilder (SharedExpr a da) (SharedExprS s0 a da v dv)
      (x, m) <- ExprMap.runTreeBuilder z'
      return (SharedExprWithMap (NodeMap.unsafeFromExprMap (ExprMap.mapmap toS  m)) x)
      --SomeValueWithNodeMap x' y <- NodeMap.runTreeBuilder z' :: (IO (SomeValueWithNodeMap (SharedExprS a da) (SharedExpr a da) v dv))
      --return (SharedExprWithMap y x')
