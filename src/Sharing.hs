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
module Sharing where

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
import NodeMap (KeySet, NodeMap, SomeNodeMap(SomeNodeMap), SomeValueWithNodeMap(..))
import qualified NodeMap
import qualified ExprRef as ExprMap

data SharedArg a da v dv where
    SharedArgVar :: SharedArg a da a da
    SharedArgExpr :: ExprName v dv -> SharedArg a da v dv

data SharedTerm a da v dv where
    SharedFunAp :: AFunction u du v dv -> SharedArg a da u du -> SharedTerm a da v dv

data SharedExpr a da v dv = (AdditiveGroup v, AdditiveGroup dv) => SharedExprSum [SharedTerm a da v dv]

data SharedExprS a da (s :: KeySet) v dv = SharedExprS (SharedExpr a da v dv)

forgetSharing2 :: forall s a v da dv. (SharedExprS a da s v dv, NodeMap s (SharedExprS a da s)) -> Expr2 a da v dv
forgetSharing2 (x, env) = goSum x
    where goSum :: forall x dx. SharedExprS a da s x dx -> Expr2 a da x dx
          goSum = \case
            SharedExprS (SharedExprSum xs) -> ExprSum (goTerm <$> xs)
          goTerm :: forall x dx. SharedTerm a da x dx -> Term2 a da x dx
          goTerm = \case
            SharedFunAp f arg -> Func2 f (goArg arg)
          goArg :: forall x dx. SharedArg a da x dx -> ExprArg a da x dx
          goArg = \case
            SharedArgVar -> ArgVar
            SharedArgExpr xname -> case NodeMap.lookup env (NodeMap.unsafeNodeKey xname) of
                y -> ArgExpr (goSum y)

goSharing2 :: forall a da v dv. Expr2 a da v dv -> TreeBuilder (SharedExpr a da) (SharedExpr a da v dv)
goSharing2 (ExprSum xs) = do
    let go' :: Term2 a da v dv -> TreeBuilder (SharedExpr a da) (SharedTerm a da v dv)
        go' = goSharing2term
    xs' <- traverse go' xs
    return $ SharedExprSum xs'

insertExpr'
  :: forall a da g v dv.
     BuildAction (Expr2 a da) g
  -> Expr2 a da v dv
  -> TreeBuilder g (ExprName v dv, g v dv)
insertExpr' x y@(ExprSum _) = insertExpr x y

goSharing2arg :: forall a da v dv. ExprArg a da v dv -> TreeBuilder (SharedExpr a da) (SharedArg a da v dv)
goSharing2arg = \case
    ArgVar -> return SharedArgVar
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr' sharingAction2 x
        return (SharedArgExpr xRef)
goSharing2term :: forall a da v dv. Term2 a da v dv -> TreeBuilder (SharedExpr a da) (SharedTerm a da v dv)
goSharing2term = \case
    Func2 f arg -> do
        arg' <- goSharing2arg arg
        return (SharedFunAp f arg')

sharingAction2 :: BuildAction (Expr2 a da) (SharedExpr a da)
sharingAction2 = BuildAction goSharing2

data SharedExprWithMap a da x dx = forall s. SharedExprWithMap (NodeMap s (SharedExprS a da s)) (SharedExprS a da s x dx)

{-
runTreeBuilder :: forall v dv s f g. (AdditiveGroup v, AdditiveGroup dv) => TreeBuilder f (g s v dv) -> IO (SomeValueWithNodeMap g f v dv)
runTreeBuilder rs_x = do
    (x, m) <- ExprMap.runTreeBuilder rs_x
    return $ SomeValueWithNodeMap x (unsafeFromExprMap m)
-}

runRecoverSharing2 :: forall a da v dv. Expr2 a da v dv -> IO (SharedExprWithMap a da v dv)
runRecoverSharing2 x = case x of
    ExprSum _ -> do
      let z = goSharing2 x :: (TreeBuilder (SharedExpr a da) (SharedExpr a da v dv))
          z' =  SharedExprS <$> z :: TreeBuilder (SharedExpr a da) (SharedExprS a da s0 v dv)
      (x, m) <- ExprMap.runTreeBuilder z'
      return (SharedExprWithMap (NodeMap.unsafeFromExprMap (ExprMap.mapmap SharedExprS  m)) x)
      --SomeValueWithNodeMap x' y <- NodeMap.runTreeBuilder z' :: (IO (SomeValueWithNodeMap (SharedExprS a da) (SharedExpr a da) v dv))
      --return (SharedExprWithMap y x')
