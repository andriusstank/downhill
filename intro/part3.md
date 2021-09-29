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

There's also a little problem with our `Expr` type.
As we're going to convert it to a graph, we
need a clear separation of nodes and edges.
`Func` is definetely an edge. `Sum` itself is a
node, but it contains a mixed bag of adjacent edges _and nodes_.
We disallow this situation by splitting AST into terms and expressions:

~~~ {.haskell}
data Term a v where
  Term :: (v -> VecBuilder u) -> Expr a u -> Term a v

data Expr a v where
  ExprVar :: Expr a a
  ExprSum :: BasicVector v => [Term a v] -> Expr a v
~~~

In order to allow sparse gradients we use `v -> VecBuilder u` instead
of plain `dv -> du` as in previous part. `BasicVector` replaces `AdditiveGroup`
in `ExprSum`.


## Inline nodes

Builders are not enough. Say we have a simple newtype wrapper for vector.
Let's have a closer look at what happens when we attempt to index it:

~~~ {.haskell}
newtype MyVector a = MyVector { unMyVector :: Vector a }

myLookup :: MyVector a -> Int -> a
myLookup v i = unMyVector v ! i
~~~

`Expr` tree will have three nodes with two edges between them:

![](./inline_fwd.dot.svg)

Let's see how gradients propagate when we flip edges:

![](./inline_back.dot.svg)

Indexing function `(! i)` produces lightweight gradient builder, as desired.
Which intermediate node promptly converts into big fat vector, ruining all
optimization!


We need a data type with ability to relay gradients without summing them.
That's what `BackGrad` is for:

~~~ {.haskell}
newtype BackGrad a v
  = BackGrad
      ( forall x.
        (x -> VecBuilder v) ->
        [Term a x]
      )
~~~

`BackGrad` turns linear functions to `Term`s. It generalizes `Expr`:

~~~ {.haskell}
realNode :: Expr a v -> BackGrad a v
realNode x = BackGrad (\f -> [Term f x])
~~~

and provides means to apply linear function without creating a node:

~~~ {.haskell}
inlineNode ::
  forall r u v.
  (VecBuilder v -> VecBuilder u) ->
  BackGrad r u ->
  BackGrad r v
inlineNode f (BackGrad g) = BackGrad go
  where
    go :: forall x. (x -> VecBuilder v) -> [Term r x]
    go h = g (f . h)
~~~

## Sparse nodes  {#sparse-nodes}

Inline nodes are still not enough. There's still no good way to access members
of tuples, or other product types for that matter. This library differentiates
unary functions `BVar a -> BVar b` only. If we have many variables to differentiate with
respect to, we have to pack them together into single tuple or record `BVar a`.
If we have a complex model, `a` might be a complex structure of nested records.

Constructing real `Expr` nodes is inefficient, because accessing any member with
have a cost proportional to the size of the whole structure. Inline nodes is not an
option, too. Accessing deeply nested members will create a long chain of `inlineNode`s.
The cost of traversing the whole chain will have to be paid every time the variable
is used. That's fine for newtypes, as wrapping is zero cost and compiler can inline it.
Wrapping gradient builders in structs with `mempty` siblings, however isn't free.
Lists, can be seen as recursively nested pairs. All this makes complexity of iterating
over the list will be $O(n^2)$. Unacceptable.

Luckily, the type of the node doesn't really matter.
Have a look at `BackGrad` definition -- there's
no `v`, only `VecBuilder`. This means we can choose a different type of node to
store gradient and hide it under `BackGrad` as if nothing happened. No one can
possibly notice. Best way to store
gradients for member access is simply store the builder, without summing it.
We have a data type for that:

~~~ {.haskell}
newtype SparseVector v = SparseVector
  { unSparseVector :: VecBuilder v }
~~~

`sumBuilder :: VecBuilder v -> SparseVector v` doesn't really sum anything,
it just stores unevaluated builders.

It might seem such sparse nodes don't do anything apart from forwarding gradients,
but that's not the case. Concatenating gradient of product types with `<>`
will collect all builders from child nodes, group them together into single struct
and pass them to parent node as a single unit. This way gradients are assembled
into a tree, making member access $O(1)$. Assuming records have reasonably small number
of direct members, of course. Behemoth records are not covered.
