# The Cool Part
## Type of the gradient
Reverse mode automatic differentiation computes gradient
for each intermediate variable. The gradient is usually given
the same type as the variable without much consideration. There
are, however, reasons to use different types.

### Dimensional analysis
Units make a great example why we might want to keep distinction
between vectors and gradients. Take an expression to compute
mass ratio of a rocket in pseudo haskell:

~~~ {.haskell}
dry, fuel :: Kg
ratio = (dry+fuel) / dry :: R
~~~

Here `dry` is the mass of the rocket, `fuel` is the mass of the fuel
both measured in kilograms. Mass ratio is a dimensionless real number. It's gradient is also dimensionless number. Let's compute gradients of `dry` and `fuel`, starting backpropagation
from `ratio`:

~~~ {.haskell}
d_ratio = 1 :: R
d_fuel = 1/dry :: Kg^(-1)
d_dry = -fuel/dry^2 :: Kg^(-1)
~~~

See? Variables have units of $\mathrm{kg}$, while their gradients
are measured in $\frac{1}{\mathrm{kg}}$. That's because gradients are _covariant_ vectors
and their units are inverse of contravariant vectors. Clearly, adding `dry` and `d_dry` makes
no sense and its nice to keep track of this distinction in types.

### Superficial Hilber space

Say a variable has a type `V`, where `V` is a vector space. We use `VectorSpace`
class from `vector-space` package for this purpose. Now if `V` happens to be Hilbert
space, there is an isomorphisms between variables and gradients. We only need to
provide an inner product on top of vector space. That's a bening requirement for
arrays of numbers, but let's try to do this with units (again, pseudo-haskell):

~~~ {.haskell}
data Rocket
  { rocketMass :: Kg
  , rocketVelocity :: Meter/Second
  }

~~~

Vector space is straightforward:

~~~ {.haskell}
Rocket m1 v1 ^+^ Rocket m2 v2 =
  Rocket (m1+m2) (v1+v2)
a *^ Rocket m v = Rocket (a*^m) (a*^v)
~~~

We need `InnerSpace` instance to make it a Hilbert space.

~~~ {.haskell}
Rocket m1 v1 <.> Rocket m2 v2 =
  (m1 <.> m2) ^+^ (v1 <.> v2)
~~~

The problem is apparent. We are adding $\frac{m^2}{s^2}$ to $\mathrm{kg}^2$!

It is possible to construct a meaningful inner product, we just need a _metric tensor_.
That's why isomorphism is not natural and it has the same character as an isomorphism
between meters and kilograms.

## TensorMul
~~~ {.haskell}
class TensorMul u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v
~~~

~~~ {.haskell}
dv ✕ v :: R
~~~
