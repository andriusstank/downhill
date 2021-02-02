{-# LANGUAGE GADTs #-}
module EType where

import Notensor (BasicVector)
data Expr4 term dv = BasicVector dv => Expr4Sum [term dv]
