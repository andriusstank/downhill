We'll need different types of linear functions for forward and reverse mode
evaluation:

~~~ {.haskell}
newtype BackFun u v = BackFun {unBackFun :: v -> VecBuilder u}
newtype FwdFun u v = FwdFun {unFwdFun :: u -> VecBuilder v}
~~~

Graph is constructed with `BackFun` edges first.
They become `FwdFun` when we flip the graph.
