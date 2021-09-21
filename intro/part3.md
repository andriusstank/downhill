# Important details

It's easy to run into quadratic complexity.

Gradients are often sparse. Consider backpropagating over `fst :: (a, b) -> a`.
If gradient of `a` is `da`, then gradient of `(a, b)` is `(da, zeroV)`. The second
element might be a large nested record of big arrays, all full of zeros! Extreme
case of gradient sparsity appears in indexing a vector. All but one elements of
gradient are zero. Constructing such a vector makes indexing $O(n)$ operation.

## Sparse gradients

Imperative implementations of backpropagation dodge this problem by updating
gradients in-place. While that's possible to do in Haskell, there's a better way --
builders. Builder is data type for efficient representation of sparse gradients.
Or it might be a `ST` action that bumps the gradient in-place.

`Downhill` library has a class for all this builder stuff:

~~~ {.haskell}
class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder :: VecBuilder v -> v
~~~

Functions on graph edges produce `VecBuilder`s. Nodes then `mconcat` them and
call `sumBuilder`.

For example, builder for pairs looks like this:

~~~ {.haskell}
type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
~~~

`Nothing` stands for zero vector. `Maybe` is important here. Data types can be nested and `Maybe`
allows to cut whole subtrees of zeros.

<!---

Vector builder type might be:

~~~ {.haskell}
data VecBuilder a
  = SingletonVector Int a
  | DenseVector (Vector a)
  | ...
~~~

`SingletonVector n x` encodes a vector that contains `x` in position `n` and
zero everywhere else. `DenseVector` is an efficient way to store gradients of
dense operations while `SingletonVector` handles indexing.
Support for efficient slicing would be nice, too, but let's
keep things simple here.


TODO: explain why Monoid, not sumBuilder :: [VecBuilder v] -> v

~~~ {.haskell}
instance BasicVector (Vector a) where
  type VecBuilder (Vector a) = DList (VecBuilder a)
  sumBuilder = runST $ ...
~~~

`DList` provides `Monoid` instance. Plain Haskell list are not good here,
because left associated concatenation of many lists has quadratic complexity.

There's a little problem: `sumBuilder` needs to produce a vector, but it has
no way to know its length -- the list of builders might be even empty. We need
length indexed vectors, but thats a different topic.
-->

## Better AST

`Expr` type in the library is different in a few ways.

First of all, it hasn't got
pairs of vectors and gradients, such as `a da v dv`. Just `a v`. Full set of
parameters was very useful to explain the idea, but only `da` and `dv` are needed
for backpropagation. That's much simpler.

We'll need different types of linear functions for forward and reverse mode
evaluation:

~~~ {.haskell}
newtype BackFun u v = BackFun {unBackFun :: v -> VecBuilder u}
newtype FwdFun u v = FwdFun {unFwdFun :: u -> VecBuilder v}
~~~

Graph is constructed with `BackFun` edges first.
They become `FwdFun` when we flip the graph.

There's also a little problem with our `Expr` type.
As we're going to convert it to a graph, we
need a clear separation of nodes and edges.
`Func` is definetely an edge. `Sum` itself is a
node, but it contains a mixed bag of adjacent edges _and nodes_.
We disallow this situation by splitting AST into terms and expressions:

~~~ {.haskell}
data Term e a v where
    Term :: e u v -> Expr e a u -> Term e a v

data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v
~~~

`e` is the type of the edge -- `BackFun` or `FwdFun`.

`BasicVector` replaces `AdditiveGroup`, as explained above.


## Inline nodes

Builders are not enough. Say we have a simple newtype wrapper for vector:

~~~ {.haskell}
newtype MyVector a = MyVector { unMyVector :: Vector a }

myLookup :: MyVector a -> Int -> a
myLookup v i = unMyVector v ! i
~~~

Newtype wrappers are expected to have (near) zero runtime cost.

A node for `unMyVector` operation will be created. It will receive single builder
for indexing operation `(! i)`, evaluate `sumBuilder` on it, which will turn
small builder into vector, full of zeros. Turns out wrapper ondoes all builder
optimizations and makes lookup $O(n)$ again!

We need a data type with ability to relay gradients without summing them.
Enter `BackGrad`:

~~~ {.haskell}
newtype BackGrad a v = BackGrad (forall x. (x -> VecBuilder v) -> [Term BackFun a x])
~~~

It generalizes  `Expr BackFun`:

~~~ {.haskell}
realNode :: Expr BackFun a v -> BackGrad a v
realNode x = BackGrad (\f -> [Term (BackFun f) x])
~~~

and provides means to apply linear function without creating a node:

~~~ {.haskell}
inlineNode :: forall r u v. (VecBuilder v -> VecBuilder u) -> BackGrad r u -> BackGrad r v
~~~


`BackGrad` is not required to construct a node. It might opt to simply
relay gradients to parent node. See `inlineNode` function in the source
code.

## Sparse nodes  {#sparse-nodes}

Inline nodes are still not enough. There's still no good way to access elements
of tuples, or other product types for that matter.
Constructing real `Expr` nodes is suboptimal, because accessing nested
elements as in `fst . fst` will destroy gradient sparsity. Inline nodes is not an
option, because there's a possibility of long chains of inline operations.
Lists, for example, can be seen as
recursively nested pairs. Iterating over the list will create a chain of inline
nodes, compiler won't inline them, because it can't inline recursive functions.
This all will make complexity of traversing a list $O(n^2)$. Unacceptable.

Luckily, the type of the node doesn't really matter.
Have a look at `BackGrad` definition -- there's
no `v`, only `VecBuilder`. This means we can choose a different type of node to
store gradient and hide it under `BackGrad` as if nothing happened.
We have a data type
`SparseVector` for that.

~~~ {.haskell}
  newtype SparseVector v = SparseVector
    { unSparseVector :: VecBuilder v }
~~~

`sumBuilder` of `SparseVector` doesn't really sum anything, it just stores unevaluated
builders.

Sparse nodes play an important role for tree shaped data structures such as
nested records. They recreate tree structure bottom up.

