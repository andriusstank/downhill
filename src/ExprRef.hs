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

newtype ExprRef b a da v dv = ExprRef (StableName Any)

newtype ExprMap f = ExprMap { unExprMap :: HashMap (StableName Any) (SomeExpr f) }

mapmap :: forall f g a da. (forall v dv. f a da v dv -> g a da v dv) -> ExprMap (f a da) -> ExprMap (g a da)
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

data SomeExpr f = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f v dv)

debugShow :: ExprRef b a v da dv -> String
debugShow (ExprRef ref) = show (hashStableName ref)

unsafeCastType''' :: SomeExpr f -> f v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookupExprRef :: ExprMap f -> ExprRef b a da v dv -> Maybe (f v dv)
lookupExprRef (ExprMap m) (ExprRef ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing

insertExprRef :: (AdditiveGroup v, AdditiveGroup dv) => ExprMap f -> ExprRef b a da v dv -> f v dv -> ExprMap f
insertExprRef (ExprMap cache') (ExprRef name) y  = ExprMap (Map.insert name (SomeExpr y) cache')

newtype TreeBuilder f r = TreeCache { unTreeCache :: StateT (ExprMap f) IO r }
    deriving (Functor, Applicative, Monad)


insertTreeCache :: (AdditiveGroup v, AdditiveGroup dv) => ExprRef b a da v dv -> f v dv -> TreeBuilder f ()
insertTreeCache name value = do
    cache' <- TreeCache get
    let newCache = insertExprRef cache' name value
    TreeCache (put newCache)

insertTreeBuilder
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprRef b a da v dv
    -> TreeBuilder f (f v dv)
    -> TreeBuilder f (ExprRef b a da v dv, f v dv)
insertTreeBuilder name value = do
    y <- value
    insertTreeCache name y
    return (name, y)

insertTreeBuilder'
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprRef b a da v dv
    -> TreeBuilder f (f v dv) -- ^ blah
    -> TreeBuilder f (ExprRef b a da v dv, f v dv)
insertTreeBuilder' name computeAction = do
    cache <- TreeCache get
    case lookupExprRef cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name computeAction

newtype BuildAction f g = BuildAction (forall v dv. (AdditiveGroup v, AdditiveGroup dv) => f v dv -> TreeBuilder g (g v dv))

lookupTree
  :: forall f g b a v da dv. (AdditiveGroup v, AdditiveGroup dv)
  => f v dv
  -> BuildAction f g
  -> TreeBuilder g (ExprRef b a da v dv, g v dv)
lookupTree expr (BuildAction value) = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    insertTreeBuilder' (ExprRef name) (value expr)

eraseType :: a -> Any
eraseType = unsafeCoerce
