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
)
where

import Expr
import GHC.StableName
import Data.VectorSpace (AdditiveGroup(..))
import Tensor
import Data.Coerce (coerce)
import GHC.Exts (Any, )
import Data.HashMap.Strict (HashMap)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (MonadIO(..))
import ExprRef
    ( debugShow,
      insertExpr,
      lookupExprName,
      mapmap, mapmapWithKey,
      runTreeBuilder,
      BuildAction(..),
      ExprMap,
      ExprName,
      TreeBuilder )
import qualified Data.HashMap.Strict as Map
--import NodeMap (unsafeNodeKey, KeySet, NodeMap, SomeNodeMap(SomeNodeMap), SomeValueWithNodeMap(..), NodeKey(..))
--import qualified NodeMap
import qualified ExprRef as ExprMap

{-
data SharedArgS a da v dv where
    SharedArgVar :: SharedArg a da a da
    SharedArgExpr :: ExprName v dv -> SharedArg a da v dv

data SharedTermS a da v dv where
    SharedFunAp :: AFunction u du v dv -> SharedArg a da u du -> SharedTerm a da v dv

data SharedExpr a da v dv = (AdditiveGroup v, AdditiveGroup dv) => SharedExprSum { unSharedExprSum :: [SharedTerm a da v dv] }
-}


-- data SharedExprS a da (s :: KeySet) v dv = SharedExprS (SharedExpr a da v dv)

-- data SharedExprS' (s :: KeySet) a da v dv = SharedExprS (SharedExpr a da v dv)

--unSharedExprS (SharedExprS x)= x
