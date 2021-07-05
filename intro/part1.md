# The Cool Part
## Type of the gradient
We focus on differentiatin a scalar valued function `f :: a -> R`.

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

We need `InnerSpace` instance to make it a Hilbert space:

~~~ {.haskell}
Rocket m1 v1 <.> Rocket m2 v2 =
  (m1 <.> m2) ^+^ (v1 <.> v2)
~~~

The problem is apparent. We are adding $\frac{m^2}{s^2}$ to $\mathrm{kg}^2$!

It is possible to construct a meaningful inner product, we just need a _metric tensor_.
That's why isomorphism is not natural and it has the same character as an isomorphism
between meters and kilograms.

## Linear functions

We're going to build abstract syntax tree and use observal sharing to combine identical
expressions. We don't, however, need to store entire computation as an AST. Recall
that derivative is a linear function. More specificially, its a linear approximation of a
function at a given point. Conal Elliott has wonderful explanation in his paper _The Simple Essence of Automatic Differentiation_ [^1]. We only need to store that linear part as an AST
and evaluate that.

Let's start with linear functions with one argument.

~~~ {.haskell}
data PrimFunc u du v dv = PrimFunc
  { fwdFun :: u -> v
  , backFun :: dv -> du
  }
~~~

`PrimFunc` is a primitive building block of linear expressions. It comes with two parts: $u \rightarrow v$ evaluates this function, while $dv \rightarrow du$ backpropagates gradient.

~~~ {.haskell}
class TensorMul u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v
~~~

Operator ✕ stands for application of linear function _and_ composition of linear functions.
That's not as crazy as it might seem. Linear functions can be represented as matrices. In this
case function applicatio is matrix-vector product, while function composition is matrix-matrix
product. Both operations are in principle the same.

TensorMul: tensor product, followed by contraction.

Gradient: gradient of vector $v$ is a function $v \rightarrow \mathbb{R}$.

Say, $u$, $v$ are vectors, $du$, $dv$ are gradients and $f$ is a function $u \leftarrow v$.

First of all, we can use `✕` to evaluate $f$:

~~~ {.haskell}
f ✕ u :: v
~~~

Next, since gradient is a linear function, we can evaluate it, too:

~~~ {.haskell}
du ✕ u :: R
dv ✕ v :: R
~~~

Finally, the reason operator `✕` was introduced at all – gradient can go on
the left side of the function:

~~~ {.haskell}
dv ✕ f :: du
~~~

A law:

`dv ✕ (f ✕ u) == (dv ✕ f) ✕ u`

This law means that backFun must be _transpose_ of fwdFun.

Note that vector `v` can also be seen as a function `(✕ v) :: dv -> R`. Both vector and its
gradient are duals of each other.


We will also need a wrapper for vectors to avoid overlapping instances and type families:

~~~ {.haskell}
newtype Vec x = Vec { unVec :: x }
~~~

`PrimFunc` instance:

~~~ {.haskell}
instance TensorMul (PrimFunc u du v dv) (Vec u) where
    type (PrimFunc u du v dv) ✕ (Vec u) = Vec v
    (PrimFunc f _) ✕ Vec v = Vec (f v)
~~~

## AST

We are ready to start building our AST:

~~~ {.haskell}
data Expr a da v dv where
    Var :: Expr a da a da
    Func :: PrimFunc u du v dv -> Expr a da u du -> Expr a da v dv
~~~

`Expr a da v dv` is a linear expression of type `v` with one free variable of type `a`.

## Linear functions with multiple arguments

There's a difference between linear and bilinear functions. Linear functions with two variables satisfy this equation:
$$f(x_1+x_2,y_1+y_2) = f(x_1, y_1) + f(x_2, y_2)$$

Multiplication, for example, is bilinear, not linear because
$$(a+b) \cdot (x+y) \ne a \cdot x + b \cdot y$$

It turns turns out any linear function can be written as a sum of one variable linear functions:
$$f(x,y) = f_1(x) + f_2(y)$$
It's easy to see equation holds by plugging $f_1(x)=f(x, 0)$, $f_2(y)=f(0, y)$.

One variable functions are already covered, so we have all the pieces to finish AST definition:

~~~ {.haskell}
data Expr a da v dv where
    Var :: Expr a da a da
    Func :: PrimFunc u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv
~~~

Thats it! That's all we need to evaluate in reverse mode.
