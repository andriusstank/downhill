{-# LANGUAGE RankNTypes #-}
module Downhill.Linear.Graph(
    Graph,
    SomeGraph(..),
    evalSomeGraph, flipSomeGraph
)
where
import Downhill.Internal.Graph.Graph
import Downhill.Linear.Expr (FwdFun, BackFun, flipBackFun)
import qualified Downhill.Internal.Graph.Graph as Graph

evalSomeGraph :: SomeGraph FwdFun a p -> a -> p
evalSomeGraph g v = case g of
    SomeGraph g' -> evalGraph g' v

flipSomeGraph :: SomeGraph BackFun a z -> SomeGraph FwdFun z a
flipSomeGraph (SomeGraph g) = SomeGraph (Graph.flipGraph flipBackFun g)
