{-# LANGUAGE TypeFamilies #-}

module Downhill.Tree.BVar (BVarField (..))
where

import Data.Kind (Type)
import Data.Singletons (SingI, type (~>))
import Downhill.BVar (BVar (BVar))
import Downhill.Tree.FieldWrapper (FieldWrapper (FieldWrapper, unFieldWrapper))
import Prelude.Singletons (ConstSym1)
import Downhill.Linear.AD (inlineNode)

class BVarField (f :: k ~> Type) where
    type FieldBVar (r :: Type) f :: k ~> Type
    splitField :: forall r sh. (SingI sh) => BVar r (FieldWrapper f sh) -> FieldWrapper (FieldBVar r f) sh
    joinField :: forall r sh. (SingI sh) => FieldWrapper (FieldBVar r f) sh -> BVar r (FieldWrapper f sh)

splitMonoLeaf :: forall r a sh. BVar r (FieldWrapper (ConstSym1 a) sh) -> FieldWrapper (ConstSym1 (BVar r a)) sh
splitMonoLeaf (BVar (FieldWrapper x) dx) = FieldWrapper (BVar x (inlineNode unFieldWrapper id dx))

joinMonoLeaf :: forall r a sh. FieldWrapper (ConstSym1 (BVar r a)) sh -> BVar r (FieldWrapper (ConstSym1 a) sh)
joinMonoLeaf (FieldWrapper (BVar x dx)) = BVar (FieldWrapper x) (inlineNode FieldWrapper id dx)

instance BVarField (ConstSym1 a) where
    type FieldBVar (r :: Type) (ConstSym1 a) = ConstSym1 (BVar r a)
    splitField = splitMonoLeaf
    joinField = joinMonoLeaf
