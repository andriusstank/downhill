{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Downhill.Linear.Backprop
  ( -- * Backpropagation
    backprop,
    -- * Graph
    buildGraph,
    --abstractBackprop,
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
  ( BasicVector (VecBuilder),
    FullVector (identityBuilder),
    SparseVector (SparseVector, unSparseVector),
    Term,
  )
import GHC.IO.Unsafe (unsafePerformIO)
import Downhill.Internal.Graph.Types ( BackFun, flipBackFun )

buildGraph ::
  forall a v.
  (BasicVector a, BasicVector v) =>
  [Term a v] ->
  SomeGraph BackFun a v
buildGraph fidentityBuilder = unsafePerformIO $ do
  og <- recoverSharing fidentityBuilder
  return (Graph.fromOpenGraph og)

abstractBackprop ::
  forall a v.
  (BasicVector a, BasicVector v) =>
  BackGrad a v ->
  (v -> VecBuilder v) ->
  v ->
  a
abstractBackprop (BackGrad f) builder x = case buildGraph [f builder] of
  SomeGraph g -> evalGraph (transposeGraph flipBackFun g) x

_backprop :: forall a v. (BasicVector a, BasicVector v) => BackGrad a v -> VecBuilder v -> a
_backprop dvar x = abstractBackprop sparseDVar unSparseVector (SparseVector x)
  where
    sparseDVar :: BackGrad a (SparseVector v)
    sparseDVar = castBackGrad dvar

backprop :: forall a v. (BasicVector a, FullVector v) => BackGrad a v -> v -> a
backprop dvar = abstractBackprop dvar identityBuilder
