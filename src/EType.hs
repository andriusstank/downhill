{-# LANGUAGE GADTs #-}
module EType where

import Notensor (BasicVector)

data Endpoint p da dv where
    SourceNode :: Endpoint p da da
    InnerNode :: p dv -> Endpoint p da dv

data VectorSum term dv = BasicVector dv => VectorSum [term dv]
