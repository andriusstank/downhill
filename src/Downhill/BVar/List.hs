module Downhill.BVar.List
where

import Downhill.Linear.Expr (BasicVector (VecBuilder, sumBuilder))
import Data.VectorSpace (AdditiveGroup)
import Data.These ()
import Data.Align ()

newtype ListBuilder a = ListBuilder [VecBuilder a]

instance Semigroup (VecBuilder a) => Semigroup (ListBuilder a) where
    ListBuilder xb <> ListBuilder yb = ListBuilder (go xb yb)
        where go [] ys = ys
              go xs [] = xs
              go (x:xs) (y:ys) = (x<>y) : go xs ys

instance Monoid (VecBuilder a) => Monoid (ListBuilder a) where
    mempty = ListBuilder []

instance BasicVector a => BasicVector [a] where
    type VecBuilder [a] = ListBuilder a
    sumBuilder (ListBuilder x) = sumBuilder <$> x

newtype FinsList a = FinsList [a]

instance AdditiveGroup a => AdditiveGroup (FinsList a) where
    zeroV = []
