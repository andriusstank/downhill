{-# LANGUAGE ExistentialQuantification #-}
module ExprRef (
    ExprRef(..),
    ExprMap(..),
    SomeExpr(..)
)
where
import GHC.StableName
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.VectorSpace (AdditiveGroup)

newtype ExprRef b a v da dv = ExprRef (StableName Any)

newtype ExprMap f b a da = ExprMap (HashMap (StableName Any) (SomeExpr f b a da))

data SomeExpr f b a da = forall v dv. (AdditiveGroup v, AdditiveGroup dv) => SomeExpr (f b a v da dv)

