{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.BVar (
    BVar (..),
    constant,
    var,
    DFun(..),
    backprop,
    ad,
)
where

import Downhill.Linear.AD (AD, Graph, identityAD)
import Downhill.Linear.AD qualified as AD
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Grad, Manifold (..))
import Downhill.VSpace (VSpace)
import NumHask.Prelude

-- | Variable is a value paired with derivative.
data BVar r a = BVar
    { bvarValue :: a
    , bvarGrad :: AD r (Tang a)
    }

instance (Additive a, HasBuilder (Tang a), HasBuilder (Grad a)) => Additive (BVar r a) where
    (BVar f0 df) + (BVar g0 dg) = BVar (f0 + g0) (df + dg)
    zero = BVar zero zero

instance
    ( Subtractive a
    , HasBuilder (Grad a)
    , HasBuilder (Tang a)
    , Subtractive (Grad a)
    , Subtractive (Tang a)
    ) =>
    Subtractive (BVar r a)
    where
    negate (BVar y0 dy) = BVar (negate y0) (negate dy)
    (BVar f0 df) - (BVar g0 dg) = BVar (f0 - g0) (df - dg)

instance
    ( Multiplicative a
    , HasBuilder (Grad a)
    , Scalar (Grad a) ~ a
    , Scalar (Tang a) ~ a
    , HasBuilder a
    , HasBuilder (Tang a)
    , VSpace (Tang a)
    ) =>
    Multiplicative (BVar r a)
    where
    (BVar f0 df) * (BVar g0 dg) = BVar (f0 * g0) (f0 *| dg + g0 *| df)
    one = BVar one zero

-- | A variable with derivative of zero.
constant :: forall r a. (HasBuilder (Grad a), HasBuilder (Tang a)) => a -> BVar r a
constant x = BVar x zero

-- | A variable with identity derivative.
var :: a -> BVar (Tang a) a
var x = BVar x identityAD

newtype DFun a b f = DFun (a -> (b, f))

backprop ::
    forall r a.
    ( HasBuilder r
    , HasBuilder (DualOf r)
    , HasBuilder (Tang a)
    , HasBuilder (Grad a)
    ) =>
    BVar r a -> (a, Graph r (Tang a))
backprop (BVar x dx) = (x, AD.ad dx)

ad ::
    forall a b.
    ( HasBuilder (Tang a)
    , HasBuilder (Grad a)
    , HasBuilder (Tang b)
    , HasBuilder (Grad b)
    ) =>
    (forall r. BVar r a -> BVar r b) ->
    DFun a b (Graph (Tang a) (Tang b))
ad f = DFun go
  where
    go :: a -> (b, Graph (Tang a) (Tang b))
    go x = backprop (f (var x))
