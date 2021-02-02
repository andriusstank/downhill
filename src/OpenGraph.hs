{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OpenGraph (
    OpenArg, OpenTerm, OpenExpr,
    runRecoverSharing4
)
where




import Expr(ExprArg(ArgExpr, ArgVar),  Term3(Func2),  Expr2, Expr3(ExprSum), Expr5(Expr5))
import Sharing (BuildAction(BuildAction), TreeBuilder)
import qualified Sharing
import Prelude hiding (lookup)
import OpenMap (OpenMap, OpenKey)
import EType (Expr4 (Expr4Sum))

type OpenArg = ExprArg OpenKey
type OpenTerm = Term3 OpenKey
--type OpenExpr = Expr3 OpenKey
type OpenExpr da = Expr4 (Term3 OpenKey da)


goSharing4 :: forall da dv. Expr5 da dv -> TreeBuilder (OpenExpr da) (OpenExpr da dv)
goSharing4 (Expr5 (Expr4Sum xs)) = do
    let go' :: Term3 (Expr5 da) da dv -> TreeBuilder (OpenExpr da) (OpenTerm da dv)
        go' = goSharing4term
    xs' <- traverse go' xs
    return $ Expr4Sum xs'

insertExpr3
  :: forall da g dv.
     BuildAction (Expr5 da) g
  -> Expr5 da dv
  -> TreeBuilder g (OpenKey dv, g dv)
insertExpr3 x y@(Expr5 (Expr4Sum _)) = do
    (k, z) <- Sharing.insertExpr x y
    return (k, z)

goSharing4arg :: forall da dv. ExprArg (Expr5 da) da dv -> TreeBuilder (OpenExpr da) (OpenArg da dv)
goSharing4arg = \case
    ArgVar -> return ArgVar 
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction4 x
        return (ArgExpr xRef)

goSharing4term :: forall da dv. Term3 (Expr5 da) da dv -> TreeBuilder (OpenExpr da) (OpenTerm da dv)
goSharing4term = \case
    Func2 f arg -> do
        arg' <- goSharing4arg arg
        return (Func2 f arg')

sharingAction4 :: BuildAction (Expr5 da) (OpenExpr da)
sharingAction4 = BuildAction goSharing4

runRecoverSharing4 :: forall da dz. Expr5 da dz -> IO (OpenExpr da dz, OpenMap (OpenExpr da))
runRecoverSharing4 x = case x of
    Expr5 (Expr4Sum _) -> do
      let z = goSharing4 x :: (TreeBuilder (OpenExpr da) (OpenExpr da dz))
      Sharing.runTreeBuilder z
