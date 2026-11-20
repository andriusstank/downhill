{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Downhill.Tree.BVar (BVarField (..), BVarRepr(..))
where

import Data.Kind (Type, Constraint)
import Data.Singletons (SingI, type (~>), Sing, type (@@))
import Downhill.BVar (BVar (BVar))
import Downhill.Tree.At (At (..))
import Prelude.Singletons (ConstSym1)
import Downhill.Linear.AD (inlineNode)

-- BVarField is obsolete
type BVarField :: forall {k}. (k ~> Type) -> Constraint
class ()  => BVarField (f :: k ~> Type) where
    type FieldBVar (r :: Type) f :: k ~> Type
    splitField :: forall r sh. (SingI sh) => BVar r (At f sh) -> At (FieldBVar r f) sh
    joinField :: forall r sh. (SingI sh) => At (FieldBVar r f) sh -> BVar r (At f sh)

splitMonoLeaf :: forall r a sh. BVar r (At (ConstSym1 a) sh) -> At (ConstSym1 (BVar r a)) sh
splitMonoLeaf (BVar (At x) dx) = At (BVar x (inlineNode unAt id dx))

joinMonoLeaf :: forall r a sh. At (ConstSym1 (BVar r a)) sh -> BVar r (At (ConstSym1 a) sh)
joinMonoLeaf (At (BVar x dx)) = BVar (At x) (inlineNode At id dx)

instance BVarField (ConstSym1 a) where
    type FieldBVar (r :: Type) (ConstSym1 a) = ConstSym1 (BVar r a)
    splitField = splitMonoLeaf
    joinField = joinMonoLeaf

type BVarRepr :: forall {k}. (k ~> Type) -> Constraint
class BVarRepr (f :: k ~> Type) where
    type ReprBVar (r :: Type) f :: k ~> Type
    splitBVarField :: forall r sh. Sing sh -> BVar r (f @@ sh) -> ReprBVar r f @@ sh
    joinBVarField :: forall r sh. Sing sh -> ReprBVar r f @@ sh -> BVar r (f @@ sh)
