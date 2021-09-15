# Important details

It's easy to run into quadratic complexity in functional setting.

Gradients are often sparse. Consider backpropagating over `fst :: (a, b) -> a`.
If gradient of `a` is `da`, gradient of `(a, b)` is `(da, zeroV)`. The second
element might be a large nested record of big arrays, all full of zeros! Extreme
case of gradient sparsity appears in indexing a vector. All but one elements of
gradient are zero. Constructing such a vector makes indexing $O(n)$ operation.

## Sparse gradients

Imperative style implementations of backpropagation dodge this problem by updating
gradients in-place. While that's possible to do in Haskell, there's a better way --
builders. Functions on graph edges would produce builders and nodes would combine
all incoming gradients at once into dense represenations.

For example, we might start with such a vector builder type:

~~~ {.haskell}
data VecBuilder a
  = SingletonVector Int a
  | DenseVector (Vector a)
~~~

`SingletonVector n x` encodes a vector that contains `x` in position `n` and
zero everywhere else. `DenseVector` is there to avoid cost of linked lists
for dense operations. Support for efficient slicing would be nice, too, but let's
keep things simple here.

Once we have collected all builders, it's perfect time to actually perform in-place
updates:

~~~ {.haskell}
sumBuilder :: [VecBuilder a] -> Vector a
sumBuilder xs = runST $ ...
~~~

`Downhill` library has a class for all this builder stuff:

~~~ {.haskell}
class Monoid (VecBuilder v) => BasicVector v where
    type VecBuilder v :: Type
    sumBuilder :: VecBuilder v -> v
~~~

Instance for vector would be like this:

~~~ {.haskell}
instance BasicVector (Vector a) where
  type VecBuilder (Vector a) = DList (VecBuilder a)
  sumBuilder = ...
~~~

`DList` provides `Monoid` instance. Plain Haskell list are not good here,
because concatenation has quadratic complexity when operations are left associated.

There's a little problem: `sumBuilder` needs to produce a vector, but it has
no way to know its length -- the list of builders might be even empty. We need
length indexed vectors, but thats a different topic.

## Better AST

`Expr` type in the library is different in a few ways.

First of all, it hasn't got
pairs of vectors and gradients, such as `a da v dv`. Just `a v`. The biggest trouble
with pairs `a da` is flipping the graph -- we would need to swap `a` and `da` in types
of _nodes_. It's a trouble, because we track the set of nodes in types.

We split AST into terms and expressions. Since we're going to convert this AST to graph, we
need a clear separation of nodes and edges. `Func` is clearly an edge. `Sum` itself is a
node, but it contains a mixed bag of adjacent edges _and nodes_.

Real AST is this:

~~~ {.haskell}
data Term e a v where
    Term :: e u v -> Expr e a u -> Term e a v

data Expr e a v where
    ExprVar :: Expr e a a
    ExprSum :: BasicVector v => [Term e a v] -> Expr e a v
~~~

It's polymorphic in type of the edge `e`. It starts with `BackFun` and becomes
`FwdFun` when flipped. This way we recover functionality we lost when 
we tossed `da` and `dv` away.

There's also `BasicVector` instead of `AdditiveGroup`, as explained above.

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

Pretty much the only thing we can do with `Expr` is to use it in `Term`
constructor. If we punch a hole there `e u v -> _ -> Term e a v`, we see
that `Expr` is a thing that turns linear functions (that's the edge `e u v`)
into `Term`s.

~~~ {.haskell}
newtype BackGrad a v = BackGrad (forall x. (x -> VecBuilder v) -> [Term BackFun a x])
~~~

Returning a list is convenient, we have a good way to construct zero by simply
returning an empty list.

It's a generalization of `Expr BackFun`:

~~~ {.haskell}
realNode :: Expr BackFun a v -> BackGrad a v
realNode x = BackGrad (\f -> [Term (BackFun f) x])
~~~

`BackGrad` is not required to construct a node. It might opt to simply
relay gradients to parent node. See `inlineNode` function in the source
code.

## Sparse nodes

Inline nodes are still not enough. There's still no good way to access elements
of tuples. Constructing real `Expr` nodes is suboptimal, because accessing nested
elements as in `fst . fst` will destroy gradient sparsity. Inline nodes is not an
options, because there's a possibility of long chains. Lists can be seen as
recursively nested pairs. Iterating over the list will create a chain of inline
nodes, compilter won't inline them, because it can't inline recursive functions.
This will make traversing a list $O(n^2)$ operation.

Type of node doesn't really matter. Have a look at `BackGrad` definition -- there's
no `v`, only `VecBuilder`. This means we can choose a different type of node to
store gradient and hide it under `BackGrad` as if nothing happened. We have a data type
`SparseVector` for that.

~~~ {.haskell}
  newtype SparseVector v = SparseVector
    { unSparseVector :: VecBuilder v }
~~~

`sumBuilder` of `SparseVector` doesn't really sum anything, it just stores unevaluated
builders.

