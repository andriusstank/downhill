We'll need different types of linear functions for forward and reverse mode
evaluation:

~~~ {.haskell}
newtype BackFun u v = BackFun {unBackFun :: v -> VecBuilder u}
newtype FwdFun u v = FwdFun {unFwdFun :: u -> VecBuilder v}
~~~

Graph is constructed with `BackFun` edges first.
They become `FwdFun` when we flip the graph.

Graph type

A set of inner nodes plus a final node.
 Initial node is special – it has no incoming edges. Final node becomes initial node
 when the edges are flipped, hence it's also special and needs to be stored separately.