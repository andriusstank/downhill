{-# LANGUAGE GADTs #-}
module EType where

import Notensor (BasicVector)

data VectorSum term dv = BasicVector dv => VectorSum [term dv]
