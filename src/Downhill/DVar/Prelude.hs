{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Downhill.DVar.Prelude (fst, snd, zip, pattern T2) where

import Downhill.DVar (BVar, DVar (DVar))
import Downhill.Linear.BackGrad (HasGrad (GradOf))
import Downhill.Linear.Expr (BasicVector)
import qualified Downhill.Linear.Prelude as Linear
import qualified Prelude

fst :: forall r a b. (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r (a, b) -> BVar r a
fst (DVar x dx) = DVar (Prelude.fst x) (Linear.fst dx)

snd :: forall r a b. (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r (a, b) -> BVar r b
snd (DVar x dx) = DVar (Prelude.snd x) (Linear.snd dx)

zip :: forall b1 b2 a. (BasicVector (GradOf b1), BasicVector (GradOf b2)) => BVar a b1 -> BVar a b2 -> BVar a (b1, b2)
zip (DVar a da) (DVar b db) = DVar (a, b) (Linear.zip da db)

toPair :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r (a, b) -> (BVar r a, BVar r b)
toPair x = (fst x, snd x)

fromPair :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r a -> BVar r b -> BVar r (a, b)
fromPair (DVar a da) (DVar b db) = DVar (a, b) (Linear.zip da db)

{-# COMPLETE T2 #-}

pattern T2 :: (BasicVector (GradOf a), BasicVector (GradOf b)) => BVar r a -> BVar r b -> BVar r (a, b)
pattern T2 a b <- (toPair -> (a, b)) where
    T2 a b = fromPair a b
