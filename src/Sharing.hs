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
import qualified Data.HashMap.Strict as Map

data SomeExpr b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (SExpr b a v da dv)

newtype ExprRef b a da = ExprRef (StableName Any)

newtype ExprMap b a da = ExprMap (HashMap (StableName Any) (SomeExpr b a da))

data SExpr b a v da dv where
    SVariable :: SExpr b a a da da
    SFunc :: (AdditiveGroup u, AdditiveGroup v, AdditiveGroup du, AdditiveGroup dv) => AFunction b u v du dv -> ExprRef b a da -> SExpr b a v da dv
    SSum :: AdditiveGroup v => [ExprRef b a da] -> SExpr b a v da dv

newtype TreeCache b a da r = TreeCache { unTreeCache :: StateT (HashMap (StableName Any) (SomeExpr b a da)) IO r }
    deriving Functor
    deriving Applicative
    deriving Monad

data SExpr' b a v da dv = SExpr' (HashMap (StableName Any) (SomeExpr b a da)) (SExpr b a v da dv)

data SomeExpr' b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr' (Expr b a v da dv)

unsafeCastType :: SExpr b a x da dx -> SExpr b a v da dv
unsafeCastType = unsafeCoerce

unsafeCastType' :: Expr b a x da dx -> Expr b a v da dv
unsafeCastType' = unsafeCoerce

unsafeCastType'' :: SomeExpr' b a da -> Expr b a v da dv
unsafeCastType'' = \case
    SomeExpr' x -> unsafeCastType' x

unsafeCastType''' :: SomeExpr b a da -> SExpr b a v da dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCastType x

unsafeLookup :: HashMap (StableName Any) (SomeExpr b a da) -> ExprRef b a da -> SExpr b a v da dv
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
        SomeExpr' e' -> unsafeCastType' e'
    where lookup' :: ExprRef b a da -> SomeExpr' b a da
          lookup' (ExprRef ref) = case Map.lookup ref m' of
              Just x -> x
              Nothing -> error ("bug: incomplete map in forgetSharing (" <> show (hashStableName ref) <> ")")
          m' :: HashMap (StableName Any) (SomeExpr' b a da)
          m' = go <$> m
          go :: SomeExpr b a da -> SomeExpr' b a da
          go = \case
            SomeExpr e' -> SomeExpr' (go' e')
          go' :: SExpr b a x da dx -> Expr b a x da dx
          go' = \case
            SVariable -> Variable
            SFunc f x -> Func f (unsafeCastType'' (lookup' x))
            SSum xs -> Sum (goSum <$> xs)
          goSum :: forall x dx. ExprRef b a da -> Expr b a x da dx
          goSum x = unsafeCastType'' . lookup' $ x

lookupTree
  :: forall b a v da dv. (AdditiveGroup v, AdditiveGroup dv)
  => Expr b a v da dv
  -> TreeCache b a da (SExpr b a v da dv) -- this will be executed only if needed
  -> TreeCache b a da (ExprRef b a da, SExpr b a v da dv)
lookupTree expr value = do
    name <- TreeCache (lift (makeStableName (eraseType expr)))
    TreeCache . lift $ putStrLn ("lookup" <> show (hashStableName name))
    cache <- TreeCache get
    case Map.lookup name cache of
        Just x -> case x of
            SomeExpr x' -> return (ExprRef name, unsafeCastType x')
        Nothing -> do
            y <- value
            cache' <- TreeCache get
            let newCache = Map.insert name (SomeExpr y) cache'
            TreeCache (lift (putStrLn ("before " <> show (hashStableName <$> Map.keys cache'))))
            TreeCache (put newCache)
            TreeCache (lift (putStrLn ("insert " <> show (hashStableName name))))
            TreeCache (lift (putStrLn ("now got " <> show (hashStableName <$> Map.keys newCache))))
            return (ExprRef name, y)

eraseType :: Expr b a v da dv -> Any
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
    (y, z) <- runStateT (unTreeCache $ recoverSharing x) Map.empty
    return (SExpr' z y)


newtype TreeCache' r = TreeCache' { unTreeCache' :: IO r }
    deriving Functor
    deriving Applicative
    deriving Monad

type TreeCache'' r = StateT () IO r 

return' :: forall r. r -> TreeCache' r
return' = coerce (return @IO @r)

runRecoverSharing' :: IO ()
runRecoverSharing' = unTreeCache' $ return' ()
--runRecoverSharing' = evalStateT (return () :: StateT () IO ()) ()
