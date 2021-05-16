{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module EType where

import Notensor ()
import Expr (BasicVector)

data Endpoint p da dv where
    SourceNode :: Endpoint p da da
    InnerNode :: p dv -> Endpoint p da dv

data Edge p f da dv where
    Edge :: f du dv -> Endpoint p da du -> Edge p f da dv

data Node p f da dv = BasicVector dv => Node [Edge p f da dv]
