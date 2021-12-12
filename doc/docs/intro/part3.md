# Sparsity

It's easy to run into quadratic complexity.

Gradients are often sparse. Consider backpropagating over `fst :: (a, b) -> a`.
If gradient of `a` is `da`, then gradient of `(a, b)` is `(da, zeroV)`. The second
element is just a zero, but it might be a very fat zero -- maybe
a large nested record of big arrays, all full of zeros! Extreme
case of gradient sparsity appears in indexing a vector. All but one elements of
gradient are zero. Constructing such a vector makes indexing $O(n)$ operation.

## Sparse gradients

Imperative implementations of backpropagation dodge this problem by updating
gradients in-place. While that's possible to do in Haskell, there's a better way --
builders. Builder is data type for efficient representation of sparse gradients.
Or it might be a `ST` action that bumps the gradient in-place if you wish.

`Downhill` library has a class for builders:

~~~ {.haskell}
class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder :: VecBuilder v -> v
~~~

`BasicVector` is absolutely minimal requirement for a type to be eligible
to automatic differentiation.
Functions on graph edges produce builders. Nodes then `mconcat` them and
pass to `sumBuilder`.

For example, builder for pairs looks like this:

~~~ {.haskell}
type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
~~~

`Nothing` stands for zero vector. `Maybe` is important here,
`mempty` wouldn't be cheap for deeply nested pairs otherwise.

## Better AST

`Expr` type in the library is different to that in previous part in a few ways.

First of all, it hasn't got
pairs of vectors and gradients, such as `a da v dv`. Just `a v`. Two
sets of parameters allows both forward and reverse mode evaluation,
but we do reverse mode only here. Those would be `da dv` for reverse
mode. We drop superfluous "d" and call them `a` anv `v`.

There's also a little problem with our `Expr` type.
As we're going to convert it to a graph, we
need a clear separation between nodes and edges.
`Func` is definetely an edge. `Sum` itself is a
node, but it contains a mixed bag of adjacent edges _and nodes_.
We disallow this situation of nodes adjacent to nodes by
splitting AST into terms and expressions:

~~~ {.haskell}
data Term a v where
  Term :: (v -> VecBuilder u) -> Expr a u -> Term a v

data Expr a v where
  ExprVar :: Expr a a
  ExprSum :: BasicVector v => [Term a v] -> Expr a v
~~~

There's `v -> VecBuilder u` in place of `PrimFunc`, which adds
support for sparse gradients and drops forward mode evaluation.
Also `BasicVector` replaces `AdditiveGroup`
in `ExprSum`.


## Inline nodes

Builders are not enough. Say, we have a simple newtype wrapper for vector.
Let's have a closer look at what happens when we attempt to index it:

~~~ {.haskell}
newtype MyVector a = MyVector { unMyVector :: Vector a }

myLookup :: MyVector a -> Int -> a
myLookup v i = unMyVector v ! i
~~~

If this code would be bluntly adapted to work on `Expr`, the
tree would have three nodes with two edges between them:

``` mermaid
graph TD
  nodeMyVector[MyVector a]
  nodeVector[Vector a]
  nodeItem[a]
  nodeVector-- "(! i)" -->nodeItem
  nodeMyVector-- unMyVector -->nodeVector
  nodeMore["..."]
  style nodeMore stroke:none,fill:none
  nodeMyVector-- "..." --> nodeMore
```

Let's see how gradients propagate when we flip edges:

``` mermaid
graph BT
  nodeMyVectorB["Grad (MyVector a)"]
  nodeMyVectorB1(["GradBuilder (MyVector a)"])
  style nodeMyVectorB1 fill:white,stroke-dasharray: 5 5
  nodeMore["..."]
  style nodeMore stroke:none,fill:none
  nodeVectorB["<b>Grad (Vector a)</b>"]
  nodeVectorB1(["GradBuilder (Vector a)"])
  style nodeVectorB1 fill:white,stroke-dasharray: 5 5
  nodeItemB["Grad a"]
  nodeItemB-- "(! i)" -->nodeVectorB1
  nodeVectorB1-- sumBuilder -->nodeVectorB
  nodeVectorB-- unMyVector -->nodeMyVectorB1
  nodeMyVectorB1-- sumBuilder -->nodeMyVectorB
  nodeMore-- "..." -->nodeMyVectorB1
```

Indexing function `(! i)` produces lightweight gradient builder, as desired.
Only for the intermediate node (labeled in bold font) to convert it into a big fat vector, undoing all
optimization!


`BackGrad` has the ability to relay gradients without summing them:

~~~ {.haskell}
newtype BackGrad a v
  = BackGrad
      ( forall x.
        (x -> VecBuilder v) ->
        Term a x
      )
~~~

`BackGrad` turns linear functions (`x -> VecBuilder v`) to `Term`s.
Alternatively, you could see it as a `Term` data constructor with a hole in
place of `Expr` argument. It generalizes `Expr`:

~~~ {.haskell}
realNode :: Expr a v -> BackGrad a v
realNode x = BackGrad (\f -> Term f x)
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
    go :: forall x. (x -> VecBuilder v) -> Term r x
    go h = g (f . h)
~~~

``` mermaid
graph BT
  nodeMyVectorB["Grad (MyVector a)"]
  nodeMyVectorB1(["GradBuilder (MyVector a)"])
  nodeMore["..."]
  style nodeMore fill:none,stroke:none
  style nodeMyVectorB1 fill:white,stroke-dasharray: 5 5
  nodeVectorB1(["GradBuilder (Vector a)"])
  style nodeVectorB1 fill:white,stroke-dasharray: 5 5
  nodeItemB["Grad a"]
  nodeItemB-- "(! i)" -->nodeVectorB1
  nodeVectorB1-- unMyVector -->nodeMyVectorB1
  nodeMyVectorB1-- sumBuilder -->nodeMyVectorB
  nodeMore-- "..." -->nodeMyVectorB1
```

Node that `Expr` is a node, `Term` is an edge. No `Expr` -- no node.


<!--
Note that `ExprSum` data constructors will turn into nodes, `Term`s will turn
into edges and everything else will be evaluated directly.
-->

## Sparse nodes

Inline nodes are still not enough. There's no good way to access members
of tuples, or other product types for that matter. They are important,
because this library differentiates
unary functions `BVar a -> BVar b` only. If we have many variables to differentiate with
respect to, we have to pack them together into single tuple or record `BVar a`.
For a complex model `a` might be a big structure of nested records.
Automatic differentiation starts with a single big variable containing all the data
and there has to be an efficient way to access all parts of it.

Constructing real `Expr` nodes won't cut, because they lose sparsity and
make the cost of accessing any member proportional to the size of the whole structure.
Inline nodes are not an
option, too. Accessing deeply nested members would create a long chain of `inlineNode`s.
The cost of traversing the whole chain will have to be paid every time the variable
is used. This way a simple traversal of a list will turn into into
a Schlemiel the painter's algorithm!

The solution is to store sparse gradients in graph nodes for this use case.
Luckily, there's no need for new types of node here.
Have a look at `BackGrad` definition -- there's
no `v`, only `VecBuilder`. This means we can choose a different type of node to
store gradient and hide it under `BackGrad` as if nothing happened. No one can
possibly notice.

~~~ {.haskell}
castBackGrad ::
  forall r v z.
  VecBuilder z ~ VecBuilder v =>
  BackGrad r v -> BackGrad r z
castBackGrad (BackGrad g) = BackGrad g
~~~

Sparse gradients are wrapped in  `SparseVector` newtype for storage in graph.
Storing naked `VecBuilder v` runs into a little problem -- what's
 `VecBuilder (VecBuilder v)`?

~~~ {.haskell}
newtype SparseVector v = SparseVector
  { unSparseVector :: VecBuilder v }
~~~

`sumBuilder :: VecBuilder v -> SparseVector v` doesn't really sum anything,
it just stores unevaluated builders.

How does it differ from inline nodes? Turns out monoid operation of builders
of product types plays a key role in intermediate nodes.
It collects gradients from all successor nodes and packs them into a
tuple/record before passing them to parent node as a single unit.
This way gradients are assembled bottom up
into a tree of the same shape as original data. Inline nodes would propagate
gradients form each leaf node all the way to the root individually.
