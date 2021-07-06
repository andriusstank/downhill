{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Downhill.Linear.Graph(
    Graph,
    SomeGraph(..),
    evalSomeGraph, flipSomeGraph,
    buildSomeGraph, backprop
)
where
import Downhill.Internal.Graph.Graph
    ( SomeGraph(..), Graph, evalGraph )
import Downhill.Linear.Expr (FwdFun, BackFun, flipBackFun, BasicVector, FullVector (identityBuilder))
import Downhill.Linear.BackGrad (HasDual(DualOf), BackGrad(..))
import GHC.IO.Unsafe (unsafePerformIO)
import Downhill.Internal.Graph.OpenGraph (recoverSharing)
import qualified Downhill.Internal.Graph.Graph as Graph

evalSomeGraph :: SomeGraph FwdFun a p -> a -> p
evalSomeGraph g v = case g of
    SomeGraph g' -> evalGraph g' v

flipSomeGraph :: SomeGraph BackFun a z -> SomeGraph FwdFun z a
flipSomeGraph (SomeGraph g) = SomeGraph (Graph.flipGraph flipBackFun g)

buildSomeGraph :: forall da v. (BasicVector da, FullVector (DualOf v)) => BackGrad da v -> SomeGraph BackFun da (DualOf v)
buildSomeGraph (BackGrad f) = unsafePerformIO $ do
    og <- recoverSharing (f identityBuilder)
    return (Graph.fromOpenGraph og)

backprop :: forall da v. (BasicVector da, FullVector (DualOf v)) => BackGrad da v -> DualOf v -> da
backprop = evalSomeGraph . flipSomeGraph . buildSomeGraph