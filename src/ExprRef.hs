{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ExprRef (
    SomeExpr(..),
    debugShow,

    -- * Map
    ExprName,
    ExprMap,
    SomeExprWithName(..),
    toList, fromListWith,
    lookupExprName,
    mapmap, mapmapWithKey,
    
    -- * Tree
    TreeBuilder,
    BuildAction(..),
    insertExpr,
    runTreeBuilder,
   {-
    insertExprName,
    insertTreeBuilder',
    -}
)
where
import GHC.StableName
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.VectorSpace (AdditiveGroup(..))

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

newtype ExprName v dv = ExprName (StableName Any)

-- idea: use StableName for tree builder only, use plain Int as ExprName and flat vector as ExprMap
-- idea: ExprMap is representable functor, make use of that
newtype ExprMap f = ExprMap { unExprMap :: HashMap (StableName Any) (SomeExpr f) }

mapmap :: forall f g. (forall v dv. (AdditiveGroup v, AdditiveGroup dv) => f v dv -> g v dv) -> ExprMap f -> ExprMap g
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

mapmapWithKey :: forall f g. (forall v dv. (AdditiveGroup v, AdditiveGroup dv) => ExprName v dv -> f v dv -> g v dv) -> ExprMap f -> ExprMap g
mapmapWithKey f (ExprMap x) = ExprMap (Map.mapWithKey go x)
    where go key (SomeExpr y) = SomeExpr (f (ExprName key) y)

data SomeExpr f = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f v dv)

data SomeExprWithName f = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExprWithName (ExprName v dv) (f v dv)

toList :: ExprMap f -> [SomeExprWithName f]
toList = fmap wrap . Map.toList . unExprMap
    where wrap (key, someValue) = case someValue of
            SomeExpr value -> SomeExprWithName (ExprName key) value

fromListWith :: forall f. [SomeExprWithName f] -> (SomeExpr f -> SomeExpr f -> SomeExpr f) -> ExprMap f
fromListWith xs f = ExprMap (Map.fromListWith f (go <$> xs))
    where go :: SomeExprWithName f -> (StableName Any, SomeExpr f)
          go (SomeExprWithName (ExprName xname) x) = (xname, SomeExpr x)

debugShow :: ExprName v dv -> String
debugShow (ExprName ref) = show (hashStableName ref)

unsafeCastType''' :: SomeExpr f -> f v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookupExprName :: ExprMap f -> ExprName v dv -> Maybe (f v dv)
lookupExprName (ExprMap m) (ExprName ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing

insertExprName :: (AdditiveGroup v, AdditiveGroup dv) => ExprMap f -> ExprName v dv -> f v dv -> ExprMap f
insertExprName (ExprMap cache') (ExprName name) y  = ExprMap (Map.insert name (SomeExpr y) cache')

newtype TreeBuilder f r = TreeCache { unTreeCache :: StateT (ExprMap f) IO r }
    deriving (Functor, Applicative, Monad)


insertTreeCache :: (AdditiveGroup v, AdditiveGroup dv) => ExprName v dv -> f v dv -> TreeBuilder f ()
insertTreeCache name value = do
    cache' <- TreeCache get
    let newCache = insertExprName cache' name value
    TreeCache (put newCache)

insertTreeBuilder
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprName v dv
    -> TreeBuilder f (f v dv)
    -> TreeBuilder f (ExprName v dv, f v dv)
insertTreeBuilder name value = do
    y <- value
    insertTreeCache name y
    return (name, y)

insertTreeBuilder'
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprName v dv
    -> TreeBuilder f (f v dv) -- ^ blah
    -> TreeBuilder f (ExprName v dv, f v dv)
insertTreeBuilder' name computeAction = do
    cache <- TreeCache get
    case lookupExprName cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name computeAction

newtype BuildAction f g = BuildAction (forall v dv. (AdditiveGroup v, AdditiveGroup dv) => f v dv -> TreeBuilder g (g v dv))

insertExpr
  :: forall f g v dv. (AdditiveGroup v, AdditiveGroup dv)
  => BuildAction f g
  -> f v dv
  -> TreeBuilder g (ExprName v dv, g v dv)
insertExpr (BuildAction value) expr = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    insertTreeBuilder' (ExprName name) (value expr)

eraseType :: a -> Any
eraseType = unsafeCoerce

runTreeBuilder :: (AdditiveGroup v, AdditiveGroup dv) => TreeBuilder f (g v dv) -> IO (g v dv, ExprMap f)
runTreeBuilder rs_x = runStateT (unTreeCache rs_x) (ExprMap Map.empty)
