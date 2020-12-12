{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
module ExprRef (
    ExprRef(..),
    ExprMap(..),
    SomeExpr(..),
    ExprMapBuilder(..),
    lookupExprRef,
    debugShow,
    mapmap
)
where
import GHC.StableName
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.VectorSpace (AdditiveGroup)
import Expr (Expr)
import Unsafe.Coerce (unsafeCoerce)

newtype ExprRef b a v da dv = ExprRef (StableName Any)

-- Key is stable name of the stored value
newtype ExprMapBuilder f b a da = ExprMapBuilder (HashMap (StableName Any) (SomeExpr f b a da))

-- Key is meaningless here
newtype ExprMap f b a da = ExprMap (HashMap (StableName Any) (SomeExpr f b a da))

fromBuilder :: ExprMapBuilder f b a da -> ExprMap f b a da
fromBuilder (ExprMapBuilder x) = ExprMap x

mapmap :: forall f g b a da. (forall v dv. f b a v da dv -> g b a v da dv) -> ExprMap f b a da -> ExprMap g b a da
mapmap f (ExprMap x) = ExprMap (go <$> x)
    where go (SomeExpr y) = SomeExpr (f y)

data SomeExpr f b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f b a v da dv)

--data SomeExpr' b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr' (Expr b a v da dv)

debugShow :: ExprRef b a v da dv -> String
debugShow (ExprRef ref) = show (hashStableName ref)

unsafeCastType' :: Expr b a x da dx -> Expr b a v da dv
unsafeCastType' = unsafeCoerce

unsafeCastType''' :: SomeExpr f b a da -> f b a v da dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x -- !!!

lookupExprRef :: ExprMap f b a da -> ExprRef b a v da dv -> Maybe (f b a v da dv)
lookupExprRef (ExprMap m) (ExprRef ref) =
    case Map.lookup ref m of
        Just x -> Just (unsafeCastType''' x)
        Nothing -> Nothing