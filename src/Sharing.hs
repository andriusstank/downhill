{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# language TypeApplications #-}
{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingVia #-}
{-# language GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Sharing (
    SomeExpr(..),
    -- * Tree
    TreeBuilder,
    BuildAction(..),
    insertExpr,
    runTreeBuilder,
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
import Control.Exception (evaluate)
import Types
import OpenMap (OpenKey(OpenKey), OpenMap)
import qualified OpenMap

type ExprName = StableName Any
--type ExprMap f = HashMap (StableName Any) (SomeExpr f)
type ExprMap f = OpenMap f

lookupExprName :: forall f v dv. ExprMap f -> ExprName -> Maybe (f v dv)
lookupExprName m ref = OpenMap.lookup m (OpenKey ref)

insertExprName :: (AdditiveGroup v, AdditiveGroup dv) => ExprMap f -> ExprName -> f v dv -> ExprMap f
insertExprName cache' name y = OpenMap.insert (OpenKey name) y cache'

newtype TreeBuilder f r = TreeCache { unTreeCache :: StateT (ExprMap f) IO r }
    deriving (Functor, Applicative, Monad)


insertTreeCache :: (AdditiveGroup v, AdditiveGroup dv) => ExprName -> f v dv -> TreeBuilder f ()
insertTreeCache name value = do
    cache' <- TreeCache get
    let newCache = insertExprName cache' name value
    TreeCache (put newCache)

insertTreeBuilder
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprName
    -> TreeBuilder f (f v dv)
    -> TreeBuilder f (ExprName, f v dv)
insertTreeBuilder name value = do
    y <- value
    insertTreeCache name y
    return (name, y)

insertTreeBuilder'
    :: (AdditiveGroup v, AdditiveGroup dv)
    => ExprName
    -> TreeBuilder f (f v dv) -- ^ blah
    -> TreeBuilder f (ExprName, f v dv)
insertTreeBuilder' name computeAction = do
    cache <- TreeCache get
    case lookupExprName cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name computeAction

newtype BuildAction f g = BuildAction (forall v dv. (AdditiveGroup v, AdditiveGroup dv) => f v dv -> TreeBuilder g (g v dv))

makeStableName'' :: f v dv -> IO (StableName Any)
makeStableName'' x = do
    x' <- evaluate x
    z <- makeStableName x'
    return (unsafeCoerce z)

insertExpr
  :: forall f g v dv. (AdditiveGroup v, AdditiveGroup dv)
  => BuildAction f g
  -> f v dv
  -> TreeBuilder g (ExprName, g v dv)
insertExpr (BuildAction value) expr = do
    name <- TreeCache (lift (makeStableName'' expr))
    insertTreeBuilder' name (value expr)

runTreeBuilder :: (AdditiveGroup v, AdditiveGroup dv) => TreeBuilder f (g v dv) -> IO (g v dv, ExprMap f)
runTreeBuilder rs_x = runStateT (unTreeCache rs_x) OpenMap.empty
