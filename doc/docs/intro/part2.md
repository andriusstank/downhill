# Linear computational graph

One way to do reverse mode automatic differentiation
is to create a DSL for differentiable functions and use `StableName`s to recover
sharing information and construct computational graph.
We adopt this approach, but with a twist.

## The twist

There's no need to construct a graph for the whole function to be differentiated.
Graph for local linear approximation of the function is enough.
Linear functions are much simpler than more
general differentiable functions and lend much better to the Haskell type system, as
we will see later.

This insight comes from Conal Elliott's paper *The simple essence of automatic differentiation* [^1],
where he explains how gradient is nothing else but the best local linear approximation.

The idea is to start with forward mode automatic differentiation, but construct an AST
for gradient instead of computing it immediately. Forward mode is easy:

~~~ {.haskell}
data DVar a = DVar
  { dvarValue :: a
  , dvarGrad :: Expr a
  }
~~~

Gradient is a linear function, thus `dvarGrad` is a linear expression, by construction.
Building the graph for `dvarGrad` only greatly reduces then scope
and complexity of the otherwise tricky "reverse" part of differentiation algorithm.

## Linear maps

Automatic differentiation is all about vector spaces and linear maps. Let me quickly
introduce them.

Vector spaces are covered by `vector-space` package.
Two most relevant operations are vector
addition and scalar-vector multiplication:

~~~ {.haskell}
(^+^) :: v -> v -> v
(*^) :: Scalar v -> v -> v
~~~

Linear map is a mapping $f: U \to V$, where $U$ and $V$ are vector spaces,
satisfying the following conditions:

\begin{align}
f(x+y) & = f(x) + f(y) \\
f(a x) & = a f(x)
\end{align}

While linear maps are conceptually just functions, we can't represent
all of them as Haskell functions, as that would
lead to terrible algorithmic complexity.

The best way to represent a linear map depends on the vector
spaces in question and on the nature of linear map itself. Threfore
we introduce a class for linear maps with an operator to evaluate them.
The choice of operator comes
from the fact that
linear maps can be represented as matrices (or, more generally, tensors)
and evaluation corresponds to matrix-vector product.

~~~ {.haskell}
class TensorMul u v where
  type u ✕ v :: Type
  (✕) :: u -> v -> u ✕ v
~~~

If `f` represents linear map $U \to V$ and `u :: U`,
then `f ✕ u :: V` evaluates $f(u)$.

Such a general operator wouldn't be very good for a Haskell library.
More specific functions
have better type inference, better error messages and make code easier to read
and navigate.  Operator `✕` is supposed to mean tensor product followed by contraction,
but there might be multiple sensible contractions, with no way to choose the
right one at each call site.
Anyway, it is very useful for explaining things and demonstrating that
quite a few operations are actually the same.

Laws of linear map can now be translated to Haskell:

~~~ {.haskell}
f ✕ (u ^+^ v) = f ✕ u ^+^ f ✕ v
f ✕ (a *^ u) = a *^ (f ✕ u)
~~~

Multiplication `✕` distributes over addition on the other side, too,
because linear maps form a vector space themselves:

~~~ {.haskell}
(f ^+^ g) ✕ u = f ✕ u ^+^ g ✕ u
(a *^ f) ✕ u = a *^ (f ✕ u)
~~~

A common case in backpropagation is domain of $f$ being scalar. We will name
it $\mathbb{R}$ to make this text more intuitive, though actual type of the scalar isn't
really important. Gradient of variable $u \in U$ in this case is a linear map $u^*: U \to \mathbb{R}$.
Vector space of such linear maps is said to be *dual vector space* of $U$.
Translating this to Haskell and choosing name `du` for $u^*$ gives

~~~ {.haskell}
u  :: u
du :: du
du ✕ u :: R
~~~

We use lowercase type variables `u` and `du`, because all automatic differentiation
code will be polymorphic -- `u` and `du` are type variables.
Going back to matrix analogy, if `u` is a column vectors, then
`du` is a row vector and their product is a scalar.

Here `du` can be seen not only as a (row) vector, but also as a function:

~~~ {.haskell}
(du ✕) :: u -> R
~~~

Vector `u` can be seen as a function, too:

~~~ {.haskell}
(✕ u) :: du -> R
~~~

There's a nice symmetry between `u` and `du` -- both have data representation,
both have function representation and both are duals of each other.

Another important operation besides evaluation is composition. We don't need
another operator, because `✕` fits the bill. If you see linear maps as matrices,
composition is matrix multiplication. This usage of `✕`
gives rise to associativity law.
Here are associative law of `✕` together with the laws of usual Haskell
function application and composition operators, put together to show relation between them:

~~~ {.haskell}
(f . g) $ u = f $ (g $ u)
(f ✕ g) ✕ u = f ✕ (g ✕ u)

(f . g) . h = f . (g . h)
(f ✕ g) ✕ h = f ✕ (g ✕ h)
~~~

## PrimFunc

The first ingredient of linear computational graphs is linear functions of a single argument.

~~~ {.haskell}
data PrimFunc u du v dv = PrimFunc
  { fwdFun :: u -> v
  , backFun :: dv -> du
  }
~~~

`PrimFunc` is made of two parts: `u -> v` evaluates this function,
while `dv -> du` backpropagates gradient. Given it's a linear
map, it should have `TensorMul` instance, but unfortunately we quickly run into
overlapping instances problem. We resort to newtype wrappers to overcome it.

~~~ {.haskell}
newtype Vec x = Vec { unVec :: x }
~~~

This little nuisance is a consequence of overly general `TensorMul` class. The instance
can now be given:

~~~ {.haskell}
instance TensorMul (PrimFunc u du v dv) (Vec u) where
    type (PrimFunc u du v dv) ✕ (Vec u) = Vec v
    (PrimFunc f _) ✕ Vec v = Vec (f v)
~~~

That was forward mode evaluation.
Can you guess which operator we're going to use for reverse mode?
Of course, it has to be `✕`.
There's one more way to use it -- on the *left* of the function:

~~~ {.haskell}
 f ✕ u :: v
dv ✕ f :: du
~~~

Matrix analogy goes a long way here -- if `u` and `v` are a column vectors,
`du` and `dv` are row vectors, then `f` is a matrix and `✕` is matrix-vector
or vector-matrix multiplication. Another thing worth mentioning -- there are no transpositions
of matrices in sight. Matrix transposition assumes Hilbert space, we shouldn't be expecting
them here.

Since we already have newtype wrappers for vectors, we might create a different one for
gradients.

~~~ {.haskell}
newtype Cov x = Cov {unCov :: x}
~~~

`Cov` stands for *covector*. It doesn't have much to do with variance and covariance,
it just indicates that the variable should be positioned on the left side of the function.

~~~ {.haskell}
instance TensorMul (Cov dv) (PrimFunc u du v dv) where
  type (Cov dv) ✕ (PrimFunc u du v dv) = Cov du
  Cov v ✕ (PrimFunc _ f) = Cov (f v)
~~~

Function $f$ can be seen as a bilinear form. If Haskell allowed such notation:

~~~ {.haskell}
(✕ f ✕) :: dv -> u -> R
~~~

Associative law comes into play again:

~~~ {.haskell}
dv ✕ (f ✕ u)    =     (dv ✕ f) ✕ u
dv ✕ fwdFun f u = backFun f dv ✕ u
~~~

This means `fwdFun` and `backFun` can't be arbitrary linear maps -- above
equation must hold for all choices of `dv` and `u`. Mathematically, this
law says that backFun must be *transpose* of fwdFun. That's pretty much the
definition of tranpose of a linear map.

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
$$
f(x_1+x_2,y_1+y_2) = f(x_1, y_1) + f(x_2, y_2)
$$

Multiplication, for example, is bilinear, not linear because
$$
(a+b) \cdot (x+y) \ne a \cdot x + b \cdot y
$$

It turns turns out any linear function can be written as a sum of one variable linear functions:
$$
f(x,y) = f_1(x) + f_2(y)
$$
It's easy to see equation holds by plugging $f_1(x)=f(x, 0)$, $f_2(y)=f(0, y)$.

We have all the pieces to finish AST definition:

~~~ {.haskell}
data Expr a da v dv where
    Var :: Expr a da a da
    Func :: PrimFunc u du v dv -> Expr a da u du -> Expr a da v dv
    Sum :: AdditiveGroup v => [Expr a da v dv] -> Expr a da v dv
~~~

Thats it! That's all we need to evaluate in reverse mode.

## Discussion

## Evaluation

Evaluating `Expr` directly is inefficient -- we should recover sharing
information first. Still, it gives `Expr` precise semantics, so let's do it
anyways.

`Expr a da v dv` represents a function `a -> v`, so it's natural to give it
a `TensorMul` instance. The code writes itself:

~~~ {.haskell}
instance TensorMul (Expr a da v dv) (Vec a) where
    type Expr a da v dv ✕ Vec a = Vec v
    expr ✕ a = case expr of
        Var -> a                 -- Var is identity function
        Func f v -> f ✕ (v ✕ a)  -- Func f v = f ✕ v
        Sum vs -> sumV [v ✕ a | v <- vs]
~~~

Reverse mode evaluation is also straightforward:

~~~ {.haskell}
instance AdditiveGroup da => TensorMul (Vec dv) (Expr a da v dv) where
    type Vec dv ✕ (Expr a da v dv) = Vec da
    dv ✕ expr = case expr of
        Var -> dv
        Func f v -> (dv ✕ f) ✕ v
        Sum vs -> sumV [dv ✕ v | v <- vs]
~~~

## Puzzle for the reader

We're going to build a graph and flip edges later to turn
reverse mode evaluation into forward mode. As `Expr` is a tree
can be readily be transposed, too.

~~~ {.haskell}
transposeExpr :: AdditiveGroup da => Expr a da v dv -> Expr dv v da a
transposeExpr = _
~~~

Can you figure this out? This time code doesn't write itself.


[^1]: Conal Elliott. The Simple Essence of Automatic Differentiation. [http://conal.net/papers/essence-of-ad/](http://conal.net/papers/essence-of-ad/)
