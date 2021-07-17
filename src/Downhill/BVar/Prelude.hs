{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Downhill.BVar.Prelude (pattern T2, pattern T3) where

import Downhill.DVar (BVar, DVar (DVar))
import qualified Downhill.Linear.Prelude as Linear
import Prelude ()
import Downhill.Grad (HasGrad)

toPair :: (HasGrad a, HasGrad b) => BVar r (a, b) -> (BVar r a, BVar r b)
toPair (DVar (x, y) (Linear.T2 dx dy)) = (DVar x dx, DVar y dy)

{-# COMPLETE T2 #-}

pattern T2 :: (HasGrad a, HasGrad b) => BVar r a -> BVar r b -> BVar r (a, b)
pattern T2 a b <- (toPair -> (a, b)) where
    T2 (DVar a da) (DVar b db) = DVar (a, b) (Linear.T2 da db)

toTriple :: (HasGrad a, HasGrad b, HasGrad c) => BVar r (a, b, c) -> (BVar r a, BVar r b, BVar r c)
toTriple (DVar (x, y, z) (Linear.T3 dx dy dz)) = (DVar x dx, DVar y dy, DVar z dz)

{-# COMPLETE T3 #-}

pattern T3 :: (HasGrad a, HasGrad b, HasGrad c) => BVar r a -> BVar r b -> BVar r c -> BVar r (a, b, c)
pattern T3 a b c <- (toTriple -> (a, b, c)) where
    T3 (DVar a da) (DVar b db) (DVar c dc) = DVar (a, b, c) (Linear.T3 da db dc)
