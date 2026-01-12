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
    ScalarMulFun(..),
    Metric
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
type instance DualOf (Generically v) = Generically (DualOf v)

class (a ~ DualOf (DualOf a), b ~ DualOf (DualOf b)) => LinearFun f a b | f a -> b, f b -> a where
  (<*|) :: f -> a -> b
  (|*>) :: DualOf b -> f -> DualOf a

infixr 7 <*|
infixl 7 |*>

class (LinearFun f a (DualOf a)) => Metric f a

data SomeLinearFun (a :: Type) (b :: Type) where
  SomeLinearFun ::
    forall f a b.
    (LinearFun f a b) =>
    f -> SomeLinearFun a b

instance (a ~ DualOf (DualOf a), b ~ DualOf (DualOf b)) => LinearFun (SomeLinearFun a b) a b where
  SomeLinearFun f <*| x = f <*| x
  x |*> SomeLinearFun f = x |*> f

data IdFun = IdFun

instance (a ~ DualOf (DualOf a)) => LinearFun IdFun a a where
  IdFun <*| x = x
  x |*> IdFun = x

data NegateFun = NegateFun

instance (Subtractive a, Subtractive (DualOf a), a ~ DualOf (DualOf a)) => LinearFun NegateFun a a where
  NegateFun <*| x = negate x
  x |*> NegateFun = negate x


newtype ScalarMulFun a = ScalarMulFun a

instance
  ( MultiplicativeAction v
  , MultiplicativeAction (DualOf v)
  , Scalar (DualOf v) ~ Scalar v
  , DualOf (DualOf v) ~ v
  , a ~ Scalar v
  ) =>
  LinearFun (ScalarMulFun a) v v
  where
  ScalarMulFun a <*| v = a *| v
  dv |*> ScalarMulFun a = a *| dv
