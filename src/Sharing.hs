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

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Exception (evaluate)
import Types
import OpenMap (OpenKey, OpenMap)
import qualified OpenMap

newtype TreeBuilder f r = TreeCache { unTreeCache :: StateT (OpenMap f) IO r }
    deriving (Functor, Applicative, Monad)


insertTreeCache :: OpenKey dv -> f dv -> TreeBuilder f ()
insertTreeCache name value = do
    cache' <- TreeCache get
    let newCache = OpenMap.insert name value cache'
    TreeCache (put newCache)

insertTreeBuilder
    :: OpenKey dv
    -> TreeBuilder f (f dv)
    -> TreeBuilder f (OpenKey dv, f dv)
insertTreeBuilder name value = do
    y <- value
    insertTreeCache name y
    return (name, y)

insertTreeBuilder'
    :: OpenKey dv
    -> TreeBuilder f (f dv)
    -> TreeBuilder f (OpenKey dv, f dv)
insertTreeBuilder' name computeAction = do
    cache <- TreeCache get
    case OpenMap.lookup cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name computeAction

newtype BuildAction f g = BuildAction (forall dv. f dv -> TreeBuilder g (g dv))

insertExpr
  :: BuildAction f g
  -> f dv
  -> TreeBuilder g (OpenKey dv, g dv)
insertExpr (BuildAction value) expr = do
    name <- TreeCache (lift (OpenMap.makeOpenKey expr))
    insertTreeBuilder' name (value expr)

runTreeBuilder :: forall f g dv. TreeBuilder f (g dv) -> IO (g dv, OpenMap f)
runTreeBuilder rs_x = runStateT (unTreeCache rs_x) OpenMap.empty
