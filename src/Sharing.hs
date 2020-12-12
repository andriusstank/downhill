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
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (MonadIO(..))
import ExprRef
import qualified Data.HashMap.Strict as Map

data SExpr b a da v dv where
    SVariable :: SExpr b a da a da
    SFunc :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction b u du v dv -> ExprRef b a da u du -> SExpr b a da v dv
    SSum :: AdditiveGroup v => [ExprRef b a da v dv] -> SExpr b a da v dv

--newtype TreeCache b a da r = TreeCache { unTreeCache :: StateT (HashMap (StableName Any) (SomeExpr SExpr b a da)) IO r }

--type TreeCache = TreeBuilder SExpr

data SExpr' b a da v dv = SExpr' (HashMap (StableName Any) (SomeExpr (SExpr b a da))) (SExpr b a da v dv)


unsafeCastType :: SExpr b a da x dx -> SExpr b a da v dv
unsafeCastType = unsafeCoerce

unsafeCastType' :: Expr b a da x dx -> Expr b a da v dv
unsafeCastType' = unsafeCoerce

{-
unsafeCastType'' :: SomeExpr' b a da -> Expr b a v da dv
unsafeCastType'' = \case
    SomeExpr' x -> unsafeCastType' x
-}

unsafeCastType''' :: SomeExpr (SExpr b a da) -> SExpr b a da v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCastType x

{-
unsafeLookup :: HashMap (StableName Any) (SomeExpr SExpr b a da) -> ExprRef b a v da dv -> SExpr b a v da dv
unsafeLookup m (ExprRef ref) = case Map.lookup ref m of
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

forgetSharing :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv) => SExpr' b a da v dv -> Expr b a da v dv
forgetSharing (SExpr' m e) =
    case go (SomeExpr e) of
        SomeExpr e' -> unsafeCastType' e'
    where lookup' :: forall x dx. ExprRef b a da x dx -> Expr b a da x dx -- SomeExpr' b a da
          lookup' ref = case lookupExprRef m' ref of
              Just x -> x
              Nothing -> error ("bug: incomplete map in forgetSharing (" <> debugShow ref <> ")")
          m' :: ExprMap (Expr b a da) --HashMap (StableName Any) (SomeExpr Expr b a da)
          m' = mapmap go' (ExprMap m) --go <$> m
          go :: SomeExpr (SExpr b a da) -> SomeExpr (Expr b a da)
          go = \case
            SomeExpr e' -> SomeExpr (go' e')
          go' :: SExpr b a da x dx -> Expr b a da x dx
          go' = \case
            SVariable -> Variable
            SFunc f x -> Func f (lookup' x)
            SSum xs -> Sum (goSum <$> xs)
          goSum :: forall x dx. ExprRef b a da x dx -> Expr b a da x dx
          goSum x =  lookup' x

goSharing :: forall a b v da dv. (AdditiveGroup v, AdditiveGroup dv) => Expr b a da v dv -> TreeBuilder (SExpr b a da) (SExpr b a da v dv)
goSharing expr = case expr of
    Variable -> return SVariable -- TODO: recover sharing for variables or not?
    Func f x -> do
        (xRef, _sx) <- lookupTree x (BuildAction recoverSharing)
        return (SFunc f xRef)
    Sum xs -> do
        let go' x = lookupTree x (BuildAction recoverSharing)
        xs' <- traverse go' xs
        return $ SSum (fst <$> xs')

sharingAction :: BuildAction (Expr b) (SExpr b)
sharingAction = BuildAction goSharing

recoverSharing :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv) => Expr b a da v dv -> TreeBuilder (SExpr b a da) (SExpr b a da v dv)
recoverSharing expr'' = snd <$> lookupTree expr'' sharingAction

runRecoverSharing :: (AdditiveGroup v, AdditiveGroup dv) => Expr b a da v dv -> IO (SExpr' b a da v dv)
runRecoverSharing x = do
    (y, z) <- runStateT (unTreeCache $ recoverSharing x) (ExprMap Map.empty)
    return (SExpr' (unExprMap z) y)

