{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Downhill.Linear.Fun(
    DualOf,
    LinearFun(..),
    SomeLinearFun(..),
    IdFun(..),
    NegateFun(..),
    ScalarMulFun(..)
)
where
import NumHask.Prelude

type family DualOf (v :: Type) :: Type

type instance DualOf Int = Int
type instance DualOf Integer = Integer
type instance DualOf Float = Float
type instance DualOf Double = Double
type instance DualOf (a, b) = (DualOf a, DualOf b)
type instance DualOf (a, b, c) = (DualOf a, DualOf b, DualOf c)

class LinearFun f a b | f -> a b where
  (<*|) :: f -> a -> b
  (|*>) :: DualOf b -> f -> DualOf a

infixr 7 <*|
infixl 7 |*>

data SomeLinearFun (a :: Type) (b :: Type) where
  SomeLinearFun ::
    forall f a b.
    (LinearFun f a b) =>
    f -> SomeLinearFun a b

instance LinearFun (SomeLinearFun a b) a b where
  SomeLinearFun f <*| x = f <*| x
  x |*> SomeLinearFun f = x |*> f

data IdFun (a :: Type) = IdFun

instance LinearFun (IdFun a) a a where
  IdFun <*| x = x
  x |*> IdFun = x

data NegateFun (a :: Type) = NegateFun

instance (Subtractive a, Subtractive (DualOf a)) => LinearFun (NegateFun a) a a where
  NegateFun <*| x = negate x
  x |*> NegateFun = negate x


newtype ScalarMulFun v = ScalarMulFun (Scalar v)

instance
  ( MultiplicativeAction v
  , MultiplicativeAction (DualOf v)
  , Scalar (DualOf v) ~ Scalar v
  ) =>
  LinearFun (ScalarMulFun v) v v
  where
  ScalarMulFun a <*| v = a *| v
  dv |*> ScalarMulFun a = a *| dv
