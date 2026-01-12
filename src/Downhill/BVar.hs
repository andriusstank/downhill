{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.BVar (
    BVar (..),
    constant,
    var,
    Graph,
    DFun (..), SomeDFun(..),
    backprop,
    ad,
)
where

import Downhill.Linear.AD (AD (AD), Graph, dense2, denseTerm', identityAD, realNode, realNode2, inlineNode)
import Downhill.Linear.AD qualified as AD
import Downhill.Linear.Builder (HasBuilder)
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum), Term (Term), VecPair (..))
import Downhill.Linear.Fun (DualOf, LinearFun ((<*|), (|*>)), ScalarMulFun (ScalarMulFun))
import Downhill.Manifold (Grad, HasGrad, IsScalar, Manifold, Tang)
import Downhill.VSpace (InnerSpace (..), VSpace ((|*|)), (*^))
import NumHask.Prelude
import Prelude qualified

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

instance (IsScalar a, HasGrad a) => Multiplicative (BVar r a) where
    (BVar f0 df) * (BVar g0 dg) = BVar (f0 * g0) (f0 *| dg + g0 *| df)
    one = BVar one zero

newtype VectorScalarMulFun v = VectorScalarMulFun v

instance
    ( a ~ Scalar v
    , DualOf a ~ a
    , VSpace v
    ) =>
    LinearFun (VectorScalarMulFun v) a v
    where
    VectorScalarMulFun v <*| a = a *| v
    dv |*> VectorScalarMulFun v = dv |*| v

instance
    ( IsScalar (Scalar v)
    , Tang v ~ v
    , Scalar (DualOf v) ~ Scalar v
    , VSpace v
    , VSpace (DualOf v)
    , MultiplicativeAction v
    , HasBuilder (Scalar v)
    , HasBuilder v
    , HasBuilder (DualOf v)
    ) =>
    MultiplicativeAction (BVar r v)
    where
    type Scalar (BVar r v) = BVar r (Scalar v)
    BVar v dv |* BVar a da =
        BVar
            (v |* a)
            --(dense2 (VectorScalarMulFun v) (ScalarMulFun a) da dv)
            (realNode2 [t1, t2])
                where
                    t1, t2 :: Term ('VecPair r (DualOf r)) ('VecPair v (DualOf v))
                    t1 = denseTerm' (v |*) (v |*|) da
                    t2 = denseTerm' (|* a) (|* a) dv

sqr :: (Multiplicative a) => a -> a
sqr x = x * x

rsqrt :: (ExpField a) => a -> a
rsqrt x = recip (sqrt x)

instance (IsScalar a, HasGrad a, Divisive a) => Divisive (BVar r a) where
    recip (BVar x dx) = BVar (recip x) (df *| dx)
      where
        df = negate (recip (sqr x))
    BVar x dx / BVar y dy = BVar (x / y) ((recip y *| dx) - ((x / sqr y) *| dy))

instance (IsScalar a, HasGrad a, ExpField a) => ExpField (BVar r a) where
    exp (BVar x dx) = BVar (exp x) (exp x *| dx)
    log (BVar x dx) = BVar (log x) (recip x *| dx)

instance (FromRational a, HasGrad a) => FromRational (BVar r a) where
    fromRational x = constant (fromRational x)

instance (FromInteger a, HasGrad a) => FromInteger (BVar r a) where
    fromInteger x = constant (fromInteger x)

instance (IsScalar a, HasGrad a, ExpField a, TrigField a) => TrigField (BVar r a) where
    pi = BVar pi zero
    sin (BVar x dx) = BVar (sin x) (cos x *| dx)
    cos (BVar x dx) = BVar (cos x) (negate (sin x) *| dx)
    asin (BVar x dx) = BVar (asin x) (rsqrt (one - sqr x) *| dx)
    acos (BVar x dx) = BVar (acos x) (negate (rsqrt (one - sqr x)) *| dx)
    atan (BVar x dx) = BVar (atan x) (recip (one + sqr x) *| dx)
    sinh (BVar x dx) = BVar (sinh x) (cosh x *| dx)
    cosh (BVar x dx) = BVar (cosh x) (sinh x *| dx)
    asinh (BVar x dx) = BVar (asinh x) (rsqrt (one + sqr x) *| dx)
    acosh (BVar x dx) = BVar (acosh x) (rsqrt (sqr x - one) *| dx)
    atanh (BVar x dx) = BVar (atanh x) (recip (one - sqr x) *| dx)

    -- TODO: test
    atan2 (BVar y0 dy) (BVar x0 dx) = BVar (atan2 y0 x0) ((x0 / d) *| dy - (y0 / d) *| dx)
      where
        d = (x0 ^ 2) + (y0 ^ 2)

instance (IsScalar a, Num a, HasGrad a) => Num (BVar r a) where
    (BVar f0 df) + (BVar g0 dg) = BVar (f0 + g0) (df + dg)
    (BVar f0 df) - (BVar g0 dg) = BVar (f0 - g0) (df - dg)
    (BVar f0 df) * (BVar g0 dg) = BVar (f0 Prelude.* g0) (f0 *| dg + g0 *| df)
    negate (BVar f0 df) = BVar (negate f0) (negate df)
    abs (BVar f0 df) = BVar (Prelude.abs f0) (Prelude.signum f0 *| df)
    signum (BVar f0 _) = BVar (Prelude.signum f0) zero
    fromInteger x = BVar (Prelude.fromInteger x) zero

instance (IsScalar a, Fractional a, HasGrad a) => Fractional (BVar r a) where
    fromRational x = BVar (Prelude.fromRational x) zero
    recip (BVar x dx) = BVar (Prelude.recip x) (df *| dx)
      where
        df = negate (Prelude.recip (sqr x))
    BVar x dx / BVar y dy = BVar (x Prelude./ y) ((Prelude.recip y *| dx) - ((x Prelude./ sqr y) *| dy))

prsqrt :: (Floating a) => a -> a
prsqrt x = Prelude.recip (Prelude.sqrt x)

instance (IsScalar a, Floating a, HasGrad a) => Floating (BVar r a) where
    pi = BVar Prelude.pi zero
    exp (BVar x dx) = BVar (Prelude.exp x) (Prelude.exp x *| dx)
    log (BVar x dx) = BVar (Prelude.log x) (Prelude.recip x *| dx)
    sin (BVar x dx) = BVar (Prelude.sin x) (Prelude.cos x *| dx)
    cos (BVar x dx) = BVar (Prelude.cos x) (negate (Prelude.sin x) *| dx)
    asin (BVar x dx) = BVar (Prelude.asin x) (prsqrt (1 - sqr x) *| dx)
    acos (BVar x dx) = BVar (Prelude.acos x) (negate (prsqrt (1 - sqr x)) *| dx)
    atan (BVar x dx) = BVar (Prelude.atan x) (Prelude.recip (1 + sqr x) *| dx)
    sinh (BVar x dx) = BVar (Prelude.sinh x) (Prelude.cosh x *| dx)
    cosh (BVar x dx) = BVar (Prelude.cosh x) (Prelude.sinh x *| dx)
    asinh (BVar x dx) = BVar (Prelude.asinh x) (prsqrt (1 + sqr x) *| dx)
    acosh (BVar x dx) = BVar (Prelude.acosh x) (prsqrt (sqr x - 1) *| dx)
    atanh (BVar x dx) = BVar (Prelude.atanh x) (Prelude.recip (1 - sqr x) *| dx)

type instance DualOf (BVar r v) = BVar r (DualOf v)

instance
    ( Manifold v
    , Manifold (Scalar v)
    , VSpace v
    , VSpace (DualOf v)
    , Tang v ~ v
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    ) =>
    VSpace (BVar r v)
    where
    BVar u du |*| BVar v dv = BVar (u |*| v) (realNode (ExprSum [t1, t2]))
      where
        t1 = denseTerm' (u |*|) (u |*) dv
        t2 = denseTerm' (|*| v) (*| v) du

instance
    ( Manifold v
    , Manifold (Scalar v)
    , InnerSpace v
    , Tang v ~ v
    , VSpace (DualOf v)
    , HasBuilder v
    , HasBuilder (DualOf v)
    , HasBuilder (Scalar v)
    ) =>
    InnerSpace (BVar r v)
 where
    BVar u du <.> BVar v dv = BVar (u <.> v) (realNode (ExprSum [t1, t2]))
      where
        t1 = denseTerm' (u <.>) (riesz u |*) dv
        t2 = denseTerm' (<.> v) (*| riesz v) du
    riesz (BVar u du) = BVar (riesz u) (realNode (ExprSum [denseTerm' riesz riesz du]))
    coriesz (BVar u du) = BVar (coriesz u) (realNode (ExprSum [denseTerm' coriesz coriesz du]))

-- | A variable with derivative of zero.
constant :: forall r a. (HasBuilder (Grad a), HasBuilder (Tang a)) => a -> BVar r a
constant x = BVar x zero

-- | A variable with identity derivative.
var :: a -> BVar (Tang a) a
var x = BVar x identityAD

newtype DFun a b f = DFun { unDFun :: a -> (b, f) }

data SomeDFun a b where
    SomeDFun :: (LinearFun f (Tang a) (Tang b)) => (a -> (b, f)) -> SomeDFun a b

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
