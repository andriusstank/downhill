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
import Downhill.Linear.BackGrad (HasGrad(GradOf), BackGrad(..))
import GHC.IO.Unsafe (unsafePerformIO)
import Downhill.Internal.Graph.OpenGraph (recoverSharing)
import qualified Downhill.Internal.Graph.Graph as Graph

evalSomeGraph :: SomeGraph FwdFun a p -> a -> p
evalSomeGraph g v = case g of
    SomeGraph g' -> evalGraph g' v

flipSomeGraph :: SomeGraph BackFun a z -> SomeGraph FwdFun z a
flipSomeGraph (SomeGraph g) = SomeGraph (Graph.flipGraph flipBackFun g)

buildSomeGraph :: forall a v. (BasicVector (GradOf a), FullVector (GradOf v)) => BackGrad a v -> SomeGraph BackFun (GradOf a) (GradOf v)
buildSomeGraph (BackGrad f) = unsafePerformIO $ do
    og <- recoverSharing (f identityBuilder)
    return (Graph.fromOpenGraph og)

backprop :: forall a v. (BasicVector (GradOf a), FullVector (GradOf v)) => BackGrad a v -> GradOf v -> GradOf a
backprop f = evalSomeGraph (flipSomeGraph (buildSomeGraph  f))
