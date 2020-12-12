{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ExprRef (
    ExprRef(..),
    ExprMap(..),
    SomeExpr(..),
    lookupExprRef,
    debugShow,
    mapmap,
    insertExprRef
)
where
import GHC.StableName
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.VectorSpace (AdditiveGroup)

import Unsafe.Coerce (unsafeCoerce)

newtype ExprRef b a v da dv = ExprRef (StableName Any)

newtype ExprMap f b a da = ExprMap { unExprMap :: HashMap (StableName Any) (SomeExpr f b a da) }

mapmap :: forall f g b a da. (forall v dv. f b a v da dv -> g b a v da dv) -> ExprMap f b a da -> ExprMap g b a da
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

data SomeExpr f b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f b a v da dv)

debugShow :: ExprRef b a v da dv -> String
debugShow (ExprRef ref) = show (hashStableName ref)

unsafeCastType''' :: SomeExpr f b a da -> f b a v da dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookupExprRef :: ExprMap f b a da -> ExprRef b a v da dv -> Maybe (f b a v da dv)
lookupExprRef (ExprMap m) (ExprRef ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing

insertExprRef :: (AdditiveGroup v, AdditiveGroup dv) => ExprMap f b a da -> ExprRef b a v da dv -> f b a v da dv -> ExprMap f b a da
insertExprRef (ExprMap cache') (ExprRef name) y  = ExprMap (Map.insert name (SomeExpr y) cache')
