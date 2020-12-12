{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ExprRef (
    ExprRef(..),
    ExprMap(..),
    SomeExpr(..),
    TreeBuilder(..),
    unTreeCache,
    lookupExprRef,
    debugShow,
    mapmap,
    insertExprRef,
    insertTreeBuilder',
    lookupTree,
    BuildAction(..)
)
where
import GHC.StableName
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.VectorSpace (AdditiveGroup)

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

newtype ExprRef b a v da dv = ExprRef (StableName Any)

newtype ExprMap f b a da = ExprMap { unExprMap :: HashMap (StableName Any) (SomeExpr f b a da) }

mapmap :: forall f g b a da. (forall v dv. f b a v da dv -> g b a v da dv) -> ExprMap f b a da -> ExprMap g b a da
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

data SomeExpr f b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f b a v da dv)

debugShow :: ExprRef b a v da dv -> String
debugShow (ExprRef ref) = show (hashStableName ref)

unsafeCastType''' :: SomeExpr f b a da -> f b a v da dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookupExprRef :: ExprMap f b a da -> ExprRef b a v da dv -> Maybe (f b a v da dv)
lookupExprRef (ExprMap m) (ExprRef ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing

insertExprRef :: (AdditiveGroup v, AdditiveGroup dv) => ExprMap f b a da -> ExprRef b a v da dv -> f b a v da dv -> ExprMap f b a da
insertExprRef (ExprMap cache') (ExprRef name) y  = ExprMap (Map.insert name (SomeExpr y) cache')

newtype TreeBuilder f b a da r = TreeCache { unTreeCache :: StateT (ExprMap f b a da) IO r }
    deriving (Functor, Applicative, Monad)


insertTreeCache :: (AdditiveGroup v, AdditiveGroup dv) => ExprRef b a v da dv -> f b a v da dv -> TreeBuilder f b a da ()
insertTreeCache name value = do
    cache' <- TreeCache get
    let newCache = insertExprRef cache' name value
    TreeCache (put newCache)

insertTreeBuilder
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprRef b a v da dv
    -> TreeBuilder f b a da (f b a v da dv)
    -> TreeBuilder f b a da (ExprRef b a v da dv, f b a v da dv)
insertTreeBuilder name value = do
    y <- value
    insertTreeCache name y
    return (name, y)

insertTreeBuilder'
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprRef b a v da dv
    -> TreeBuilder f b a da (f b a v da dv) -- ^ blah
    -> TreeBuilder f b a da (ExprRef b a v da dv, f b a v da dv)
insertTreeBuilder' name computeAction = do
    cache <- TreeCache get
    case lookupExprRef cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name computeAction

newtype BuildAction f g b = BuildAction (forall a v da dv. (AdditiveGroup v, AdditiveGroup dv) => f a v da dv -> TreeBuilder g b a da (g b a v da dv))

lookupTree
  :: forall g f b a v da dv. (AdditiveGroup v, AdditiveGroup dv)
  => g a v da dv
  -> BuildAction g f b
  -> TreeBuilder f b a da (ExprRef b a v da dv, f b a v da dv)
lookupTree expr (BuildAction value) = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    insertTreeBuilder' (ExprRef name) (value expr)

eraseType :: a -> Any
eraseType = unsafeCoerce
