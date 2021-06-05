{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Downhill.BVar.Prelude (pattern T2) where

import Downhill.DVar (BVar, DVar (DVar))
import Downhill.Linear.BackGrad (HasGrad (GradOf))
import Downhill.Linear.Expr (BasicVector)
import qualified Downhill.Linear.Prelude as Linear
import Prelude ()

toPair :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r (a, b) -> (BVar r a, BVar r b)
toPair (DVar (x, y) (Linear.T2 dx dy)) = (DVar x dx, DVar y dy)

{-# COMPLETE T2 #-}

pattern T2 :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r a -> BVar r b -> BVar r (a, b)
pattern T2 a b <- (toPair -> (a, b)) where
    T2 (DVar a da) (DVar b db) = DVar (a, b) (Linear.T2 da db)
