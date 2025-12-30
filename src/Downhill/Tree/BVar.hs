{-# LANGUAGE TypeFamilies #-}

module Downhill.Tree.BVar (BVarField (..))
where

import Data.Kind (Type)
import Data.Singletons (SingI, type (~>))
import Downhill.BVar (BVar)
import Downhill.Tree.FieldWrapper (FieldWrapper)

class BVarField (f :: k ~> Type) where
    type FieldBVar (r :: Type) f :: k ~> Type
    splitField :: forall r sh. (SingI sh) => BVar r (FieldWrapper f sh) -> FieldWrapper (FieldBVar r f) sh
    joinField :: forall r sh. (SingI sh) => FieldWrapper (FieldBVar r f) sh -> BVar r (FieldWrapper f sh)
