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
import Downhill.Internal.Graph.Types (BackFun, flipBackFun)
import Downhill.Linear.BackGrad (BackGrad (..), castBackGrad)
import Downhill.Linear.Expr
  ( BasicVector (VecBuilder),
    FullVector (identityBuilder),
    SparseVector (SparseVector, unSparseVector),
    Term,
  )
import GHC.IO.Unsafe (unsafePerformIO)

buildGraph ::
  forall a v.
  (BasicVector a, BasicVector v) =>
  [Term a v] ->
  IO (SomeGraph BackFun a v)
buildGraph fidentityBuilder = do
  og <- recoverSharing fidentityBuilder
  return (Graph.unsafeFromOpenGraph og)

abstractBackprop ::
  forall a u v.
  (BasicVector a, BasicVector u, BasicVector v) =>
  BackGrad a u ->
  (v -> VecBuilder u) ->
  v ->
  a
abstractBackprop (BackGrad f) builder x =
  case unsafePerformIO (buildGraph [f builder]) of
    SomeGraph g -> evalGraph (transposeGraph flipBackFun g) x

_backprop :: forall a v. (BasicVector a, BasicVector v) => BackGrad a v -> VecBuilder v -> a
_backprop dvar x =
  abstractBackprop @a @(SparseVector v) @(SparseVector v)
    sparseDVar
    unSparseVector
    (SparseVector x)
  where
    sparseDVar :: BackGrad a (SparseVector v)
    sparseDVar = castBackGrad dvar

-- | Purity of this function depends on laws of arithmetic
-- and linearity law of 'Term'. If your addition is approximately
-- associative, then this function is approximately pure. Fair?
backprop :: forall a v. (BasicVector a, FullVector v) => BackGrad a v -> v -> a
backprop dvar = abstractBackprop dvar identityBuilder
