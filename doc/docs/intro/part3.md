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

`Expr` type in the library is different to that in previous part in a few ways.

First of all, it hasn't got
pairs of vectors and gradients, such as `a da v dv`. Just `a v`. Full set of
parameters was useful to explain the idea, but only `da` and `dv` are needed
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

``` mermaid
graph TD
  nodeMyVector[MyVector a]
  nodeVector[Vector a]
  nodeItem[a]
  nodeVector-- "(! i)" -->nodeItem
  nodeMyVector-- unMyVector -->nodeVector
  nodeMore["..."]
  nodeMyVector-- "..." --> nodeMore
```

Let's see how gradients propagate when we flip edges:

``` mermaid
graph BT
  nodeMyVectorB["Grad (MyVector a)"]
  nodeMyVectorB1(["GradBuilder (MyVector a)"])
  nodeMore["..."]
  style nodeMyVectorB1 fill:white,stroke-dasharray: 5 5
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
Only for the intermediate node to convert it into a big fat vector, undoing all
optimization!


We need a data type with ability to relay gradients without summing them.
That's what `BackGrad` is for:

~~~ {.haskell}
newtype BackGrad a v
  = BackGrad
      ( forall x.
        (x -> VecBuilder v) ->
        Term a x
      )
~~~

`BackGrad` turns linear functions (`x -> VecBuilder v`) to `Term`s.
It generalizes `Expr`:

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
    go :: forall x. (x -> VecBuilder v) -> [Term r x]
    go h = g (f . h)
~~~

``` mermaid
graph BT
  nodeMyVectorB["Grad (MyVector a)"]
  nodeMyVectorB1(["GradBuilder (MyVector a)"])
  nodeMore["..."]
  style nodeMyVectorB1 fill:white,stroke-dasharray: 5 5
  nodeVectorB1(["GradBuilder (Vector a)"])
  style nodeVectorB1 fill:white,stroke-dasharray: 5 5
  nodeItemB["Grad a"]
  nodeItemB-- "(! i)" -->nodeVectorB1
  nodeVectorB1-- unMyVector -->nodeMyVectorB1
  nodeMyVectorB1-- sumBuilder -->nodeMyVectorB
  nodeMore-- "..." -->nodeMyVectorB1
```

## Sparse nodes  {#sparse-nodes}

Inline nodes are still not enough. There's still no good way to access members
of tuples, or other product types for that matter. This library differentiates
unary functions `BVar a -> BVar b` only. If we have many variables to differentiate with
respect to, we have to pack them together into single tuple or record `BVar a`.
If we have a complex model, `a` might be a complex structure of nested records.

Constructing real `Expr` nodes is inefficient, because accessing any member will
have a cost proportional to the size of the whole structure. Inline nodes is not an
option, too. Accessing deeply nested members will create a long chain of `inlineNode`s.
The cost of traversing the whole chain will have to be paid every time the variable
is used. That's fine for newtypes, as wrapping is zero cost and compiler can inline it.
Wrapping gradient builders in structs with `mempty` siblings, however, isn't free.
Lists can be seen as recursively nested pairs. All this makes complexity of iterating
over the list will be $O(n^2)$. That's unacceptable.

Luckily, the type of the node doesn't really matter.
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

Best way to store
gradients for member access is simply store the builder, without summing it.

~~~ {.haskell}
newtype SparseVector v = SparseVector
  { unSparseVector :: VecBuilder v }
~~~

`sumBuilder :: VecBuilder v -> SparseVector v` doesn't really sum anything,
it just stores unevaluated builders.

It might seem such sparse nodes don't do anything apart from forwarding builders,
just like `inlineNode` does,
but there's an important difference: sparse node
folds gradients from all successor nodes with `mconcat` before passing them to parent node.
Have a look at builder type of a pair:

~~~ {.haskell}
type VecBuilder (a, b) = Maybe (VecBuilder a, VecBuilder b)
~~~

It has standard monoid instance. Folding a list of such builders with
monoid operation will regroup them and pack into a single unit.
This way gradients are recursively assembled
into a tree of the same shape as original data
and each member access has $O(1)$ cost. Assuming records have reasonably small number
of direct members, of course.

