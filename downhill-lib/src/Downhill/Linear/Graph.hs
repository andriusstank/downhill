{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Downhill.Linear.Graph
  ( Graph,
    SomeGraph (..),
    evalSomeGraph,
    flipSomeGraph,
    buildSomeGraph,
    backpropGraph,
    backprop,
    backprop',
  )
where

import Downhill.Internal.Graph.Graph
  ( Graph,
    SomeGraph (..),
    evalGraph,
    flipGraph,
  )
import qualified Downhill.Internal.Graph.Graph as Graph
import Downhill.Internal.Graph.OpenGraph (recoverSharing)
import Downhill.Linear.BackGrad (BackGrad (..))
import Downhill.Linear.Expr
  ( BackFun,
    BasicVector (VecBuilder),
    FullVector (identityBuilder),
    FwdFun,
    SparseVector (SparseVector, unSparseVector),
    flipBackFun, Term
  )
import GHC.IO.Unsafe (unsafePerformIO)

evalSomeGraph :: SomeGraph FwdFun a p -> a -> p
evalSomeGraph g v = case g of
  SomeGraph g' -> evalGraph g' v

flipSomeGraph :: SomeGraph BackFun a z -> SomeGraph FwdFun z a
flipSomeGraph (SomeGraph g) = SomeGraph (Graph.flipGraph flipBackFun g)

buildSomeGraph ::
  forall a v.
  (BasicVector a, BasicVector v) =>
  [Term BackFun a v] ->
  SomeGraph BackFun a v
buildSomeGraph fidentityBuilder = unsafePerformIO $ do
  og <- recoverSharing fidentityBuilder
  return (Graph.fromOpenGraph og)

backpropGraph :: SomeGraph BackFun a z -> z -> a
backpropGraph someGraph x = case someGraph of
  SomeGraph g -> evalGraph (flipGraph flipBackFun g) x

backprop :: forall a v. (BasicVector a, BasicVector v) => BackGrad a v -> VecBuilder v -> a
backprop (BackGrad f) x = backpropGraph g (SparseVector x)
  where
    g :: SomeGraph BackFun a (SparseVector v)
    g = buildSomeGraph (f unSparseVector)

{-# ANN backprop' "HLint: ignore Eta reduce" #-}
backprop' :: forall a v. (BasicVector a, FullVector v) => BackGrad a v -> v -> a
backprop' (BackGrad f) x = backpropGraph g x
  where
    g :: SomeGraph BackFun a v
    g = buildSomeGraph (f identityBuilder)
