{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Downhill.Linear.Backprop
  ( -- * Backpropagation
    backprop,
    backprop',
    buildSomeGraph,
    abstractBackprop,
  )
where

import Downhill.Internal.Graph.Graph
  ( SomeGraph (..),
    evalGraph,
    transposeGraph,
  )
import qualified Downhill.Internal.Graph.Graph as Graph
import Downhill.Internal.Graph.OpenGraph (recoverSharing)
import Downhill.Linear.BackGrad (BackGrad (..), castBackGrad)
import Downhill.Linear.Expr
  ( BackFun,
    BasicVector (VecBuilder),
    FullVector (identityBuilder),
    SparseVector (SparseVector, unSparseVector),
    Term,
    flipBackFun,
  )
import GHC.IO.Unsafe (unsafePerformIO)

buildSomeGraph ::
  forall a v.
  (BasicVector a, BasicVector v) =>
  [Term a v] ->
  SomeGraph BackFun a v
buildSomeGraph fidentityBuilder = unsafePerformIO $ do
  og <- recoverSharing fidentityBuilder
  return (Graph.fromOpenGraph og)

abstractBackprop ::
  forall a v.
  (BasicVector a, BasicVector v) =>
  BackGrad a v ->
  (v -> VecBuilder v) ->
  v ->
  a
abstractBackprop (BackGrad f) builder x = case buildSomeGraph (f builder) of
  SomeGraph g -> evalGraph (transposeGraph flipBackFun g) x

backprop :: forall a v. (BasicVector a, BasicVector v) => BackGrad a v -> VecBuilder v -> a
backprop dvar x = abstractBackprop sparseDVar unSparseVector (SparseVector x)
  where sparseDVar :: BackGrad a (SparseVector v)
        sparseDVar = castBackGrad dvar

backprop' :: forall a v. (BasicVector a, FullVector v) => BackGrad a v -> v -> a
backprop' dvar = abstractBackprop dvar identityBuilder
