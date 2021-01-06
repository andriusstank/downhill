{-# LANGUAGE ExistentialQuantification #-}
module Types where

data SomeExpr f = forall v dv. SomeExpr (f v dv)

