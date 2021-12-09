{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Downhill.BVar.Prelude
  ( -- * Tuples

    -- | Pattern synonyms @T2@, @T3@ pack and unpack tuples:
    --
    -- @
    -- fstBVar :: (HasGrad a, HasGrad b) => BVar r (a, b) -> BVar r a
    -- fstBVar (T2 a _b) = a
    --
    -- tieBVar :: (HasGrad a, HasGrad b) => BVar r a -> BVar r b -> BVar r (a, b)
    -- tieBVar a b = T2 a b
    -- @
    pattern T2,
    pattern T3,
  )
where

import Downhill.BVar (BVar (BVar))
import Downhill.Grad (HasGrad)
import qualified Downhill.Linear.Prelude as Linear
import Prelude ()

toPair :: (HasGrad a, HasGrad b) => BVar r (a, b) -> (BVar r a, BVar r b)
toPair (BVar (x, y) (Linear.T2 dx dy)) = (BVar x dx, BVar y dy)

{-# COMPLETE T2 #-}

pattern T2 :: (HasGrad a, HasGrad b) => BVar r a -> BVar r b -> BVar r (a, b)
pattern T2 a b <-
  (toPair -> (a, b))
  where
    T2 (BVar a da) (BVar b db) = BVar (a, b) (Linear.T2 da db)

toTriple :: (HasGrad a, HasGrad b, HasGrad c) => BVar r (a, b, c) -> (BVar r a, BVar r b, BVar r c)
toTriple (BVar (x, y, z) (Linear.T3 dx dy dz)) = (BVar x dx, BVar y dy, BVar z dz)

{-# COMPLETE T3 #-}

pattern T3 :: (HasGrad a, HasGrad b, HasGrad c) => BVar r a -> BVar r b -> BVar r c -> BVar r (a, b, c)
pattern T3 a b c <-
  (toTriple -> (a, b, c))
  where
    T3 (BVar a da) (BVar b db) (BVar c dc) = BVar (a, b, c) (Linear.T3 da db dc)
