{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OpenGraph
    ( OpenKey(..), OpenMap(..), OpenArg, OpenTerm, OpenExpr, OpenExprWithMap(..)
    , SomeOpenItem(SomeOpenItem)
    , mapmap, mapmapWithKey, lookup, toList
    , runRecoverSharing4
    )
where

import Expr(ExprArg(ArgExpr, ArgVar),  Expr2(Expr2), Term3(Func2),  Expr2, Expr3(ExprSum))
import GHC.StableName (StableName(StableName))
import GHC.Base (Any)
import Sharing (SomeExpr(SomeExpr), ExprName, BuildAction(BuildAction), TreeBuilder)
import qualified Sharing
import Data.HashMap.Lazy (HashMap)
import Prelude hiding (lookup)
import qualified Data.HashMap.Lazy as HashMap

newtype OpenKey x dx = OpenKey (StableName Any)
newtype OpenMap f = OpenMap { unOpenMap :: HashMap (StableName Any) (SomeExpr f) }

data SomeOpenItem f = forall x dx. SomeOpenItem (OpenKey x dx) (f x dx)

type OpenArg = ExprArg OpenKey
type OpenTerm = Term3 OpenKey
type OpenExpr = Expr3 OpenKey

goSharing4 :: forall a da v dv. Expr2 a da v dv -> TreeBuilder (OpenExpr a da) (OpenExpr a da v dv)
goSharing4 (Expr2 (ExprSum xs)) = do
    let go' :: Term3 (Expr2 a da) a da v dv -> TreeBuilder (OpenExpr a da) (OpenTerm a da v dv)
        go' = goSharing4term
    xs' <- traverse go' xs
    return $ ExprSum xs'

insertExpr3
  :: forall s a da g v dv.
     BuildAction (Expr2 a da) g
  -> Expr2 a da v dv
  -> TreeBuilder g (OpenKey v dv, g v dv)
insertExpr3 x y@(Expr2 (ExprSum _)) = do
    (ref, z) <- Sharing.insertExpr x y
    return (OpenKey ref, z)

goSharing4arg :: forall s a da v dv. ExprArg (Expr2 a da) a da v dv -> TreeBuilder (OpenExpr a da) (OpenArg a da v dv)
goSharing4arg = \case
    ArgVar -> return ArgVar 
    ArgExpr  x ->  do
        (xRef, _sx) <- insertExpr3 sharingAction4 x
        return (ArgExpr xRef)

goSharing4term :: forall s a da v dv. Term3 (Expr2 a da) a da v dv -> TreeBuilder (OpenExpr a da) (OpenTerm a da v dv)
goSharing4term = \case
    Func2 f arg -> do
        arg' <- goSharing4arg arg
        return (Func2 f arg')

sharingAction4 :: BuildAction (Expr2 a da) (OpenExpr a da)
sharingAction4 = BuildAction goSharing4

data OpenExprWithMap a da z dz =
    OpenExprWithMap (OpenMap (OpenExpr a da)) (OpenExpr a da z dz)

runRecoverSharing4 :: forall a da v dv. Expr2 a da v dv -> IO (OpenExprWithMap a da v dv)
runRecoverSharing4 x = case x of
    Expr2 (ExprSum _) -> do
      let z = goSharing4 x :: (TreeBuilder (OpenExpr a da) (OpenExpr a da v dv))
      (x', m) <- Sharing.runTreeBuilder z
      return (OpenExprWithMap (OpenMap m) x')

mapmap :: forall f g. (forall v dv. f v dv -> g v dv) -> OpenMap f -> OpenMap g
mapmap f = OpenMap . Sharing.mapmap f . unOpenMap

mapmapWithKey :: forall f g. (forall x dx. OpenKey x dx -> f x dx -> g x dx) -> OpenMap f -> OpenMap g
mapmapWithKey f = OpenMap . Sharing.mapmapWithKey f' . unOpenMap
    where f' :: forall x dx. ExprName -> f x dx -> g x dx
          f' key = f (OpenKey key)

lookup :: OpenMap f -> OpenKey x dx -> Maybe (f x dx)
lookup (OpenMap m) (OpenKey k) = Sharing.lookupExprName m k

-- data SomeOpenItem f = forall x dx. SomeOpenItem (OpenKey x dx) (f x dx)
toList :: OpenMap f -> [SomeOpenItem f]
toList = fmap wrap . HashMap.toList . unOpenMap
    where wrap :: (StableName Any, SomeExpr f) -> SomeOpenItem f
          wrap (key, x) = case x of
              SomeExpr x' -> SomeOpenItem (OpenKey key) x'
