{-# LANGUAGE TypeFamilies #-}
module Downhill.Manifold(Manifold(..), MScalar, Grad)

where
import NumHask.Prelude
import Downhill.VSpace (VSpace)
import Downhill.Linear.Fun (DualOf)

type MScalar p = Scalar (Tang p)

class
  ( VSpace (Tang p)
  , Subtractive (MScalar p)
  , Subtractive (Tang p)
  , Subtractive (Grad p)
  ) =>
  Manifold p
  where
  type Tang p :: Type

  (.+~^) :: p -> Tang p -> p
  (.-~^) :: p -> Tang p -> p

type Grad p = DualOf (Tang p)
