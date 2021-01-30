{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    runRecoverSharing4
)
where

import Expr(ExprArg(ArgExpr, ArgVar),  Expr2(Expr2), Term3(Func2),  Expr2, Expr3(ExprSum))
import Sharing (BuildAction(BuildAction), TreeBuilder)
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)

type OpenArg = ExprArg OpenKey
type OpenTerm = Term3 OpenKey
type OpenExpr = Expr3 OpenKey


goSharing4 :: forall a da dv. Expr2 a da dv -> TreeBuilder (OpenExpr a da) (OpenExpr a da dv)
goSharing4 (Expr2 (ExprSum xs)) = do
    let go' :: Term3 (Expr2 a da) a da dv -> TreeBuilder (OpenExpr a da) (OpenTerm a da dv)
        go' = goSharing4term
    xs' <- traverse go' xs
    return $ ExprSum xs'

insertExpr3
  :: forall a da g dv.
     BuildAction (Expr2 a da) g
  -> Expr2 a da dv
  -> TreeBuilder g (OpenKey dv, g dv)
insertExpr3 x y@(Expr2 (ExprSum _)) = do
    (k, z) <- Sharing.insertExpr x y
    return (k, z)

goSharing4arg :: forall a da dv. ExprArg (Expr2 a da) a da dv -> TreeBuilder (OpenExpr a da) (OpenArg a da dv)
goSharing4arg = \case
    ArgVar -> return ArgVar 
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction4 x
        return (ArgExpr xRef)

goSharing4term :: forall a da dv. Term3 (Expr2 a da) a da dv -> TreeBuilder (OpenExpr a da) (OpenTerm a da dv)
goSharing4term = \case
    Func2 f arg -> do
        arg' <- goSharing4arg arg
        return (Func2 f arg')

sharingAction4 :: BuildAction (Expr2 a da) (OpenExpr a da)
sharingAction4 = BuildAction goSharing4

runRecoverSharing4 :: forall a da dz. Expr2 a da dz -> IO (OpenExpr a da dz, OpenMap (OpenExpr a da))
runRecoverSharing4 x = case x of
    Expr2 (ExprSum _) -> do
      let z = goSharing4 x :: (TreeBuilder (OpenExpr a da) (OpenExpr a da dz))
      Sharing.runTreeBuilder z
