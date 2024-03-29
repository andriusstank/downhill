# On the type of the gradient
We focus on differentiating a scalar valued function `f :: V -> R`,
where `V` is a vector space over real numbers `R`.

Reverse mode automatic differentiation computes gradients
for all intermediate variables. The gradient is usually given
the same type as the variable itself. This is obvious thing to do
when you see all variables as numbers or arrays of numbers.
Yet we can do better by making good use of the type system.

Mixing variables and gradients generally makes no sense.
Code is more readable and easier to write if we give them
different types.
As we flip the edges of computational graph, variables
and gradients kind of swap roles. That would be incredibly confusing
if compiler didn't help tracking which is which.

## Dimensional analysis
Units make a great example of why we might want to keep distinction
between variables and their gradients. Take an expression to compute
mass ratio of a rocket as example:

~~~ {.haskell}
dry, fuel :: Kg
ratio = (dry+fuel) / dry :: R
~~~

Here `dry` is the mass of empty rocket, `fuel` is the mass of the fuel,
both measured in kilograms. Mass ratio, as well as its gradient, are
dimensionless real numbers.
It was intentionally chosen so, in order to avoid circular reasoning.
Let's compute gradients of `dry` and `fuel`, starting backpropagation
from `ratio`:

~~~ {.haskell}
d_ratio = 1 :: R
d_fuel = 1/dry :: Kg^(-1)
d_dry = -fuel/dry^2 :: Kg^(-1)
~~~

See? Variables have units of $\mathrm{kg}$, while their gradients
are measured in $\frac{1}{\mathrm{kg}}$. That's because variables are
_contravariant_ vectors, while gradients are _covariant_ vectors.
Their units are inverse of each other.

## Superficial Hilbert space

Use of the same type `V` for both variables gradients is
typically justified by Riesz representation theorem.

Gradient of a variable `x :: V` is fundamentally a
linear function of type `V -> R`. Of course, such a representation is
unworkable -- we need to store gradients as numbers and actually compute sums
during reverse accumulation. Otherwise we will run into combinatorial
explosion of repeated computations.

Now if `V` happens to be Hilbert space, then `V -> R` is isomorphic to `V`.
For our purposes Hilbert space requirement boils down
to existance of a well behaved inner product
`(<.>) :: V -> V -> R`.
Seems to be a benign requirement -- any finite
dimensional vector space over real numbers can be equipped with one.

We have a solid theory behind using `V` type for gradients. Nonetheless,
we saw units don't match. How come?

Turns out we can't take inner product for granted. Record with variety of
units shows this clearly:

~~~ {.haskell}
data Rocket = Rocket
  { rocketMass :: Kg
  , rocketVelocity :: Meter/Second
  }

-- InnerSpace comes from vector-space package.
instance InnerSpace Rocket where
  Rocket m1 v1 <.> Rocket m2 v2 = (m1 <.> m2) ^+^ (v1 <.> v2)
~~~

We are adding $\frac{m^2}{s^2}$ to $\mathrm{kg}^2$ and expecting to get a
dimensionless scalar! Although it is possible to provide an `InnerSpace Rocket`
instance -- all we need to do
is to drop units -- but we'd very much rather not to. Isomorphism grounded on
such an instance is akin to isomorphism between kilograms and meters -- it does
indeed exist, but we wouldn't want to invoke it automatically.

## Gradient descent

Having established the rule of no mixing of variables and gradients, we
see that gradient descent is in outright violation of this rule:
$$
\mathbf{a}_{n+1} = \mathbf{a}_n-\gamma\nabla F(\mathbf{a}_n)
$$

We need a _metric tensor_ to relate gradients and variables here.
It's logical when you think about it -- gradient descent moves in the
direction of the steepest
descent. Ability to measure distances is needed to make any sense
of steepness.

Metric tensor brings different units into commensurable quantities.
It also plays the role of preconditioner. Therefore, we opt to
passing metric tensor explicitly instead of demanding existance
of canonical one via Hilber space.

<!--
Sometimes there is 
There are infinitely many choices of metric tensor. 
Demanding `V` to be
a Hilbert space means choosing a canonical one. Instead, we opt to
passing metric tensor explicitly where needed. Importantly,
differentiation does not need one.
-->