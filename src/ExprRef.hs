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

newtype ExprMap f a da = ExprMap { unExprMap :: HashMap (StableName Any) (SomeExpr f a da) }

mapmap :: forall f g a da. (forall v dv. f a v da dv -> g a v da dv) -> ExprMap f a da -> ExprMap g a da
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

data SomeExpr f a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f a v da dv)

debugShow :: ExprRef b a v da dv -> String
debugShow (ExprRef ref) = show (hashStableName ref)

unsafeCastType''' :: SomeExpr f a da -> f a v da dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookupExprRef :: ExprMap f a da -> ExprRef b a v da dv -> Maybe (f a v da dv)
lookupExprRef (ExprMap m) (ExprRef ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing

insertExprRef :: (AdditiveGroup v, AdditiveGroup dv) => ExprMap f a da -> ExprRef b a v da dv -> f a v da dv -> ExprMap f a da
insertExprRef (ExprMap cache') (ExprRef name) y  = ExprMap (Map.insert name (SomeExpr y) cache')

newtype TreeBuilder f a da r = TreeCache { unTreeCache :: StateT (ExprMap f a da) IO r }
    deriving (Functor, Applicative, Monad)


insertTreeCache :: (AdditiveGroup v, AdditiveGroup dv) => ExprRef b a v da dv -> f a v da dv -> TreeBuilder f a da ()
insertTreeCache name value = do
    cache' <- TreeCache get
    let newCache = insertExprRef cache' name value
    TreeCache (put newCache)

insertTreeBuilder
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprRef b a v da dv
    -> TreeBuilder f a da (f a v da dv)
    -> TreeBuilder f a da (ExprRef b a v da dv, f a v da dv)
insertTreeBuilder name value = do
    y <- value
    insertTreeCache name y
    return (name, y)

insertTreeBuilder'
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprRef b a v da dv
    -> TreeBuilder f a da (f a v da dv) -- ^ blah
    -> TreeBuilder f a da (ExprRef b a v da dv, f a v da dv)
insertTreeBuilder' name computeAction = do
    cache <- TreeCache get
    case lookupExprRef cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name computeAction

newtype BuildAction f g = BuildAction (forall a v da dv. (AdditiveGroup v, AdditiveGroup dv) => f a v da dv -> TreeBuilder g a da (g a v da dv))

lookupTree
  :: forall f g b a v da dv. (AdditiveGroup v, AdditiveGroup dv)
  => f a v da dv
  -> BuildAction f g
  -> TreeBuilder g a da (ExprRef b a v da dv, g a v da dv)
lookupTree expr (BuildAction value) = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    insertTreeBuilder' (ExprRef name) (value expr)

eraseType :: a -> Any
eraseType = unsafeCoerce
