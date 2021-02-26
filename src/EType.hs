{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module EType where

import Notensor (BasicVector(..))
import Data.AdditiveGroup (AdditiveGroup)
import Data.VectorSpace (VectorSpace)

data Endpoint p da dv where
    SourceNode :: Endpoint p da da
    InnerNode :: p dv -> Endpoint p da dv

data Edge p f da dv where
    Edge :: f du dv -> Endpoint p da du -> Edge p f da dv

data Node p f da dv = BasicVector dv => Node [Edge p f da dv]

instance BasicVector dv => BasicVector (Node p f da dv) where
    type VecBuilder (Node p f da dv) = Edge p f da dv
    sumBuilder = Node
