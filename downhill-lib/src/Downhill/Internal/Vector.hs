{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Downhill.Internal.Vector
  (
  )
where

import Control.Monad.ST (ST, runST)
import Data.DList (DList)
import Data.Foldable (traverse_)
import qualified Data.Vector as Unsized
import qualified Data.Vector.Generic.Mutable as UnsizedMutable
import Data.Vector.Generic.Mutable.Sized (fromSized, set, unsafeNew)
import qualified Data.Vector.Generic.Sized as VGS
--import Data.Vector.Sized
import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import GHC.TypeLits (KnownNat, Nat)

data Slice (n :: Nat) a = Slice Int Int (Unsized.Vector (VecBuilder a))

instance
  ( KnownNat n,
    BasicVector a
  ) =>
  BasicVector (VGS.Vector Unsized.Vector n a)
  where
  type VecBuilder (VGS.Vector Unsized.Vector n a) = DList (Slice n a)
  sumBuilder slices = sumBuilder <$> runST mbuilders
    where
      mbuilders :: forall s. ST s (VGS.Vector Unsized.Vector n (VecBuilder a))
      mbuilders = do
        x <- unsafeNew
        set x mempty
        let v = fromSized x
            addSlice:: Slice n a -> ST s ()
            addSlice (Slice from count builders) = traverse_ bump [0 .. count -1]
              where
                bump :: Int -> ST s ()
                bump i = UnsizedMutable.modify v (<> builders Unsized.! i) (from + i)
        traverse_ addSlice slices
        VGS.unsafeFreeze x

--let x = Vector.replicate mempty :: Vector v n (VecBuilder a)
--unsafeFreeze x