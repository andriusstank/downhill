{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Downhill.Orphans (
    GScalar,
    FromJust,
    GAdditive, GSubtractive, GMultAction
) where

import GHC.Generics (Generic (from, to))
import GHC.TypeError (ErrorMessage (Text))
import GHC.TypeLits (TypeError)
import NumHask.Prelude hiding (from, to)

class GAdditive f where
    gzero :: f p
    gplus :: f p -> f p -> f p

instance (Additive a) => GAdditive (K1 i a) where
    gzero = K1 zero
    gplus (K1 x) (K1 y) = K1 (x + y)

instance (GAdditive f) => GAdditive (M1 i m f) where
    gzero = M1 gzero
    gplus (M1 x) (M1 y) = M1 (gplus x y)

instance (GAdditive f, GAdditive g) => GAdditive (f :*: g) where
    gzero = gzero :*: gzero
    gplus (x1 :*: x2) (y1 :*: y2) = gplus x1 y1 :*: gplus x2 y2

instance GAdditive U1 where
    gzero = U1
    gplus _ _ = U1

class (GAdditive f) => GSubtractive f where
    gnegate :: f p -> f p
    gminus :: f p -> f p -> f p

instance (Subtractive a) => GSubtractive (K1 i a) where
    gnegate (K1 x) = K1 (negate x)
    gminus (K1 x) (K1 y) = K1 (x - y)

instance (GSubtractive f) => GSubtractive (M1 i m f) where
    gnegate (M1 x) = M1 (gnegate x)
    gminus (M1 x) (M1 y) = M1 (gminus x y)

instance (GSubtractive f, GSubtractive g) => GSubtractive (f :*: g) where
    gnegate (x :*: y) = gnegate x :*: gnegate y
    gminus (x1 :*: x2) (y1 :*: y2) = gminus x1 y1 :*: gminus x2 y2

instance GSubtractive U1 where
    gnegate _ = U1
    gminus _ _ = U1

instance (Generic a, GAdditive (Rep a)) => Additive (Generically a) where
    zero = Generically (to gzero)

    Generically x + Generically y = Generically (GHC.Generics.to (gplus (from x) (from y)))

instance (Generic a, GSubtractive (Rep a)) => Subtractive (Generically a) where
    negate (Generically x) = Generically (to (gnegate (from x)))

    Generically x - Generically y = Generically (GHC.Generics.to (gminus (from x) (from y)))

class GMultiplicative f where
    gone :: f p
    gmul :: f p -> f p -> f p

instance (Multiplicative a) => GMultiplicative (K1 i a) where
    gone = K1 one
    gmul (K1 x) (K1 y) = K1 (x * y)

instance (GMultiplicative f) => GMultiplicative (M1 i m f) where
    gone = M1 gone
    gmul (M1 x) (M1 y) = M1 (gmul x y)

instance (GMultiplicative f, GMultiplicative g) => GMultiplicative (f :*: g) where
    gone = gone :*: gone
    gmul (x1 :*: x2) (y1 :*: y2) = gmul x1 y1 :*: gmul x2 y2

instance GMultiplicative U1 where
    gone = U1
    gmul _ _ = U1

type family CommonScalar (a :: Maybe Type) (b :: Maybe Type) :: Maybe Type where
    CommonScalar ('Just a) _ = 'Just a
    CommonScalar 'Nothing b = b

type family GScalar f :: Maybe Type where
    GScalar (K1 i a) = 'Just (Scalar a)
    GScalar (M1 i m f) = GScalar f
    GScalar (f :*: g) = CommonScalar (GScalar f) (GScalar g)
    GScalar U1 = 'Nothing

class GMultAction a f where
    gvmul :: f p -> a -> f p

instance (MultiplicativeAction v, a ~ Scalar v) => GMultAction a (K1 i v) where
    gvmul (K1 v) a = K1 (v |* a)

instance (GMultAction a f) => GMultAction a (M1 i m f) where
    gvmul (M1 x) a = M1 (gvmul x a)

instance (GMultAction a f, GMultAction a g) => GMultAction a (f :*: g) where
    gvmul (x1 :*: x2) a = gvmul x1 a :*: gvmul x2 a

instance GMultAction a U1 where
    gvmul _ _ = U1

type family FromJust (a :: Maybe Type) :: Type where
    FromJust ('Just x) = x
    FromJust 'Nothing = TypeError ('Text "Can't deduce scalar type")

instance
    ( Generic a
    , GMultiplicative (Rep a)
    ) =>
    Multiplicative (Generically a)
    where
    Generically v * Generically a = Generically (to (gmul (from v) (from a)))
    one = Generically (to gone)

instance
    ( Generic a
    , Multiplicative (FromJust (GScalar (Rep a)))
    , GMultAction (FromJust (GScalar (Rep a))) (Rep a)
    ) =>
    MultiplicativeAction (Generically a)
    where
    type Scalar (Generically a) = FromJust (GScalar (Rep a))
    Generically v |* a = Generically (to (gvmul (from v) a))

instance
    ( MultiplicativeAction a
    , MultiplicativeAction b
    , MultiplicativeAction c
    , Scalar a ~ Scalar b
    , Scalar a ~ Scalar c
    ) =>
    MultiplicativeAction (a, b, c)
    where
    type Scalar (a, b, c) = Scalar a
    (u, v, w) |* a = (u |* a, v |* a, w |* a)

deriving via (Generically (a, b)) instance (Additive a, Additive b) => Additive (a, b)
deriving via (Generically (a, b, c)) instance (Additive a, Additive b, Additive c) => Additive (a, b, c)
deriving via (Generically (a, b)) instance (Subtractive a, Subtractive b) => Subtractive (a, b)
deriving via (Generically (a, b, c)) instance (Subtractive a, Subtractive b, Subtractive c) => Subtractive (a, b, c)
deriving via
    (Generically (a, b))
    instance
        (MultiplicativeAction a, MultiplicativeAction b, Scalar a ~ Scalar b) =>
        MultiplicativeAction (a, b)

instance MultiplicativeAction Int where
    type Scalar Int = Int
    (|*) = (*)

instance MultiplicativeAction Integer where
    type Scalar Integer = Integer
    (|*) = (*)

instance MultiplicativeAction Float where
    type Scalar Float = Float
    (|*) = (*)

instance MultiplicativeAction Double where
    type Scalar Double = Double
    (|*) = (*)
