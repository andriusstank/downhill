module Downhill.Tree.FieldWrapper (
    FieldWrapper (..),
)
where

import Data.Singletons (type (@@))

newtype FieldWrapper field sh = FieldWrapper {unFieldWrapper :: field @@ sh}
