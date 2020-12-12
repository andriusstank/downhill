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
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (MonadIO(..))
import ExprRef
import qualified Data.HashMap.Strict as Map

data SExpr b a v da dv where
    SVariable :: SExpr b a a da da
    SFunc :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction b u v du dv -> ExprRef b a u da du -> SExpr b a v da dv
    SSum :: AdditiveGroup v => [ExprRef b a v da dv] -> SExpr b a v da dv

--newtype TreeCache b a da r = TreeCache { unTreeCache :: StateT (HashMap (StableName Any) (SomeExpr SExpr b a da)) IO r }
newtype TreeBuilder f b a da r = TreeCache { unTreeCache :: StateT (ExprMap f b a da) IO r }
    deriving Functor
    deriving Applicative
    deriving Monad

type TreeCache = TreeBuilder SExpr

data SExpr' b a v da dv = SExpr' (HashMap (StableName Any) (SomeExpr SExpr b a da)) (SExpr b a v da dv)


unsafeCastType :: SExpr b a x da dx -> SExpr b a v da dv
unsafeCastType = unsafeCoerce

unsafeCastType' :: Expr b a x da dx -> Expr b a v da dv
unsafeCastType' = unsafeCoerce

{-
unsafeCastType'' :: SomeExpr' b a da -> Expr b a v da dv
unsafeCastType'' = \case
    SomeExpr' x -> unsafeCastType' x
-}

unsafeCastType''' :: SomeExpr SExpr b a da -> SExpr b a v da dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCastType x

unsafeLookup :: HashMap (StableName Any) (SomeExpr SExpr b a da) -> ExprRef b a v da dv -> SExpr b a v da dv
unsafeLookup m (ExprRef ref) = case Map.lookup ref m of
    Just x -> unsafeCastType''' x
    Nothing -> error ("bug: incomplete map in forgetSharing (" <> show (hashStableName ref) <> ")")

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

forgetSharing :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv) => SExpr' b a v da dv -> Expr b a v da dv
forgetSharing (SExpr' m e) =
    case go (SomeExpr e) of
        SomeExpr e' -> unsafeCastType' e'
    where lookup' :: forall x dx. ExprRef b a x da dx -> Expr b a x da dx -- SomeExpr' b a da
          lookup' ref = case lookupExprRef m' ref of
              Just x -> x
              Nothing -> error ("bug: incomplete map in forgetSharing (" <> debugShow ref <> ")")
          m' :: ExprMap Expr b a da --HashMap (StableName Any) (SomeExpr Expr b a da)
          m' = mapmap go' (ExprMap m) --go <$> m
          go :: SomeExpr SExpr b a da -> SomeExpr Expr b a da
          go = \case
            SomeExpr e' -> SomeExpr (go' e')
          go' :: SExpr b a x da dx -> Expr b a x da dx
          go' = \case
            SVariable -> Variable
            SFunc f x -> Func f (lookup' x)
            SSum xs -> Sum (goSum <$> xs)
          goSum :: forall x dx. ExprRef b a x da dx -> Expr b a x da dx
          goSum x =  lookup' x

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
    -> TreeBuilder f b a da (f b a v da dv)
    -> TreeBuilder f b a da (ExprRef b a v da dv, f b a v da dv)
insertTreeBuilder' name value = do
    cache <- TreeCache get
    case lookupExprRef cache name of
        Just x -> return (name, x)
        Nothing -> insertTreeBuilder name value

lookupTree
  :: forall f b a v da dv. (AdditiveGroup v, AdditiveGroup dv)
  => Expr b a v da dv
  -> TreeBuilder f b a da (f b a v da dv) -- this will be executed only if needed
  -> TreeBuilder f b a da (ExprRef b a v da dv, f b a v da dv)
lookupTree expr value = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    insertTreeBuilder' (ExprRef name) value

eraseType :: f b a v da dv -> Any
eraseType = unsafeCoerce

recoverSharing :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv) => Expr b a v da dv -> TreeCache b a da (SExpr b a v da dv)
recoverSharing expr = snd <$> lookupTree expr go
  where go :: TreeCache b a da (SExpr b a v da dv)
        go = case expr of
          Variable -> return SVariable -- TODO: recover sharing for variables or not?
          Func f x -> do
              (xRef, _sx) <- lookupTree x (recoverSharing x)
              return (SFunc f xRef)
          Sum xs -> do
              let go' x = lookupTree x (recoverSharing x)
              xs' <- traverse go' xs
              return $ SSum (fst <$> xs')

runRecoverSharing :: (AdditiveGroup v, AdditiveGroup dv) => Expr b a v da dv -> IO (SExpr' b a v da dv)
runRecoverSharing x = do
    (y, z) <- runStateT (unTreeCache $ recoverSharing x) (ExprMap Map.empty)
    return (SExpr' (unExprMap z) y)
