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
import qualified Data.HashMap.Strict as Map

data SharedArg b a da v dv where
    SharedArgVar :: SharedArg b a da a da
    SharedArgExpr :: ExprName v dv -> SharedArg b a da v dv

data SharedTerm b a da v dv where
    SharedFunAp :: AFunction b u du v dv -> SharedArg b a da u du -> SharedTerm b a da v dv

data SharedExpr b a da v dv = (AdditiveGroup v, AdditiveGroup dv) => SharedExprSum [SharedTerm b a da v dv]

--data ExprWithEnv b a da v dv = ExprWithEnv (SharedExpr b a da v dv) (ExprMap (SharedExpr b a da))

data SExpr b a da v dv where
    SVariable :: SExpr b a da a da
    SFunc :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction b u du v dv -> ExprName u du -> SExpr b a da v dv
    SSum :: AdditiveGroup v => [ExprName v dv] -> SExpr b a da v dv


--newtype TreeCache b a da r = TreeCache { unTreeCache :: StateT (HashMap (StableName Any) (SomeExpr SExpr b a da)) IO r }

--type TreeCache = TreeBuilder SExpr

-- data SExpr' b a da v dv = SExpr' (HashMap (StableName Any) (SomeExpr (SExpr b a da))) (SExpr b a da v dv)

{-
unsafeCastType :: SExpr b a da x dx -> SExpr b a da v dv
unsafeCastType = unsafeCoerce

unsafeCastType' :: Expr b a da x dx -> Expr b a da v dv
unsafeCastType' = unsafeCoerce
-}

{-
unsafeCastType'' :: SomeExpr' b a da -> Expr b a v da dv
unsafeCastType'' = \case
    SomeExpr' x -> unsafeCastType' x
-}
{-
unsafeCastType''' :: SomeExpr (SExpr b a da) -> SExpr b a da v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCastType x
-}
{-
unsafeLookup :: HashMap (StableName Any) (SomeExpr SExpr b a da) -> ExprName b a v da dv -> SExpr b a v da dv
unsafeLookup m (ExprName ref) = case Map.lookup ref m of
    Just x -> unsafeCastType''' x
    Nothing -> error ("bug: incomplete map in forgetSharing (" <> show (hashStableName ref) <> ")")
-}

{-
unsafeApply :: forall b a u v da du dv. AFunction b u v du dv -> SomeExpr b a da -> v
unsafeApply f x = f ⊗ (unsafeCastType''' x :: u)

instance TensorProduct (SExpr' b a v da dv) a v where
    (SExpr' env expr) ⊗ a = case expr of
        SVariable -> a
        SFunc f x -> f ⊗ (x' ⊗ a)
            where x' = SExpr' env (unsafeLookup env x)
        SSum xs -> sumV [x ⊗ a | x <- xs]
-}

forgetSharing :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv) => (SExpr b a da v dv, ExprMap (SExpr b a da)) -> Expr b a da v dv
forgetSharing (e, m) = go' e
    where lookup' :: forall x dx. ExprName x dx -> Expr b a da x dx -- SomeExpr' b a da
          lookup' ref = case lookupExprName m' ref of
              Just x -> x
              Nothing -> error ("bug: incomplete map in forgetSharing (" <> debugShow ref <> ")")
          m' :: ExprMap (Expr b a da)
          m' = mapmap go' m
          go' :: SExpr b a da x dx -> Expr b a da x dx
          go' = \case
            SVariable -> Variable
            SFunc f x -> Func f (lookup' x)
            SSum xs -> Sum (lookup' <$> xs)

forgetSharing2 :: forall b a v da dv. (SharedExpr b a da v dv, ExprMap (SharedExpr b a da)) -> Expr2 b a da v dv
forgetSharing2 (x, env) = goSum x
    where goSum :: forall x dx. SharedExpr b a da x dx -> Expr2 b a da x dx
          goSum = \case
            SharedExprSum xs -> ExprSum (goTerm <$> xs)
          goTerm :: forall x dx. SharedTerm b a da x dx -> Term2 b a da x dx
          goTerm = \case
            SharedFunAp f arg -> Func2 f (goArg arg)
          goArg :: forall x dx. SharedArg b a da x dx -> ExprArg b a da x dx
          goArg = \case
            SharedArgVar -> ArgVar
            SharedArgExpr xname -> case lookupExprName env xname of
                Just y -> ArgExpr (goSum y)
                Nothing -> error ("bug: incomplete map in forgetSharing (" <> debugShow xname <> ")")
          

goSharing :: forall a b v da dv. (AdditiveGroup v, AdditiveGroup dv) => Expr b a da v dv -> TreeBuilder (SExpr b a da) (SExpr b a da v dv)
goSharing expr = case expr of
    Variable -> return SVariable -- TODO: recover sharing for variables or not?
    Func f x -> do
        (xRef, _sx) <- insertExpr sharingAction x
        return (SFunc f xRef)
    Sum xs -> do
        let go' x = insertExpr sharingAction x
        xs' <- traverse go' xs
        return $ SSum (fst <$> xs')

goSharing2 :: forall b a da v dv. Expr2 b a da v dv -> TreeBuilder (SharedExpr b a da) (SharedExpr b a da v dv)
goSharing2 (ExprSum xs) = do
    let go' :: Term2 b a da v dv -> TreeBuilder (SharedExpr b a da) (SharedTerm b a da v dv)
        go' = goSharing2term
    xs' <- traverse go' xs
    return $ SharedExprSum xs'

insertExpr'
  :: forall b a da g v dv.
     BuildAction (Expr2 b a da) g
  -> Expr2 b a da v dv
  -> TreeBuilder g (ExprName v dv, g v dv)
insertExpr' x y@(ExprSum _) = insertExpr x y

goSharing2arg :: forall b a da v dv. ExprArg b a da v dv -> TreeBuilder (SharedExpr b a da) (SharedArg b a da v dv)
goSharing2arg = \case
    ArgVar -> return SharedArgVar
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr' sharingAction2 x
        return (SharedArgExpr xRef)
goSharing2term :: forall b a da v dv. Term2 b a da v dv -> TreeBuilder (SharedExpr b a da) (SharedTerm b a da v dv)
goSharing2term = \case
    Func2 f arg -> do
        arg' <- goSharing2arg arg
        return (SharedFunAp f arg')

--goSharing' :: forall a b v da dv. (AdditiveGroup v, AdditiveGroup dv) =>

sharingAction :: BuildAction (Expr b a da) (SExpr b a da)
sharingAction = BuildAction goSharing

sharingAction2 :: BuildAction (Expr2 b a da) (SharedExpr b a da)
sharingAction2 = BuildAction goSharing2


recoverSharing :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv) => Expr b a da v dv -> TreeBuilder (SExpr b a da) (SExpr b a da v dv)
recoverSharing expr'' = snd <$> insertExpr sharingAction expr''


runRecoverSharing :: (AdditiveGroup v, AdditiveGroup dv) => Expr b a da v dv -> IO (SExpr b a da v dv, ExprMap (SExpr b a da))
runRecoverSharing = runTreeBuilder . goSharing

runRecoverSharing2 :: forall b a da v dv. Expr2 b a da v dv -> IO (SharedExpr b a da v dv, ExprMap (SharedExpr b a da))
runRecoverSharing2 x = case x of
    ExprSum _ -> runTreeBuilder @v @dv (goSharing2 x)
