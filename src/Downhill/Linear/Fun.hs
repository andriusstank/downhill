{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Downhill.Linear.Fun(
    DualOf,
    LinearMap(..),
    SomeLinearMap(..),
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
type instance DualOf (Complex a) = Complex a

class (a ~ DualOf (DualOf a), b ~ DualOf (DualOf b)) => LinearMap f a b | f a -> b, f b -> a where
  (<*|) :: f -> a -> b
  (|*>) :: DualOf b -> f -> DualOf a

infixr 7 <*|
infixl 7 |*>

class (LinearMap f a (DualOf a)) => Metric f a

data SomeLinearMap (a :: Type) (b :: Type) where
  SomeLinearMap ::
    forall f a b.
    (LinearMap f a b) =>
    f -> SomeLinearMap a b

instance (a ~ DualOf (DualOf a), b ~ DualOf (DualOf b)) => LinearMap (SomeLinearMap a b) a b where
  SomeLinearMap f <*| x = f <*| x
  x |*> SomeLinearMap f = x |*> f

data IdFun = IdFun

instance (a ~ DualOf (DualOf a)) => LinearMap IdFun a a where
  IdFun <*| x = x
  x |*> IdFun = x

data NegateFun = NegateFun

instance (Subtractive a, Subtractive (DualOf a), a ~ DualOf (DualOf a)) => LinearMap NegateFun a a where
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
  LinearMap (ScalarMulFun a) v v
  where
  ScalarMulFun a <*| v = a *| v
  dv |*> ScalarMulFun a = a *| dv
