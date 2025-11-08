{-# LANGUAGE TypeFamilies #-}
module Downhill.Linear.Expr(
    VecPair(..), TangType, GradType,
    BiFun(..), Term(..), Expr(..),
    idBiFun
)
where
import Data.Kind(Type)
import Downhill.Linear.Builder(HasBuilder(..))

data VecPair = VecPair Type Type

type family TangType (w :: VecPair) :: Type where
  TangType ('VecPair v dv) = v
type family GradType (w :: VecPair) :: Type where
  GradType ('VecPair v dv) = dv

data BiFun (u :: VecPair) (v :: VecPair) = BiFun
  { fwd :: TangType u -> VecBuilder (TangType v)
  , back :: GradType v -> VecBuilder (GradType u)
  }

data Term a v where
  Term :: BiFun u v -> Expr a u -> Term a v

-- | @Expr a v@ represents a linear expression of type @v@, containing some free variables of type @a@.
type Expr :: VecPair -> VecPair -> Type
data Expr a v where
  ExprVar :: Expr a a
  ExprSum :: (HasBuilder (TangType v), HasBuilder (GradType v)) => [Term a v] -> Expr a v


idBiFun :: (HasBuilder (TangType u), HasBuilder (GradType u)) => BiFun u u
idBiFun = BiFun identityBuilder identityBuilder

