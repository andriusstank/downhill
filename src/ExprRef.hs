{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ExprRef (
    ExprName,
    ExprMap(..),
    SomeExpr(..),
    TreeBuilder(..),
    lookupExprName,
    debugShow,
    mapmap,
    insertExprName,
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

newtype ExprName v dv = ExprName (StableName Any)

newtype ExprMap f = ExprMap { unExprMap :: HashMap (StableName Any) (SomeExpr f) }

mapmap :: forall f g a da. (forall v dv. f a da v dv -> g a da v dv) -> ExprMap (f a da) -> ExprMap (g a da)
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

data SomeExpr f = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f v dv)

debugShow :: ExprName da dv -> String
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

lookupTree
  :: forall f g v dv. (AdditiveGroup v, AdditiveGroup dv)
  => f v dv
  -> BuildAction f g
  -> TreeBuilder g (ExprName v dv, g v dv)
lookupTree expr (BuildAction value) = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    insertTreeBuilder' (ExprName name) (value expr)

eraseType :: a -> Any
eraseType = unsafeCoerce
