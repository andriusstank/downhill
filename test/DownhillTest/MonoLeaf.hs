{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.MonoLeaf (MonoLeaf, Field)
where

import Data.Singletons (Apply, SingI (sing), type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.BVar (BVar (..))
import Downhill.Linear.AD (inlineNode)
import Downhill.Linear.Builder (HasBuilder (identityBuilder), VecBuilder, sumBuilder)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Tree.FieldWrapper (FieldDualOf, FieldScalar, FieldTang, FieldWrapper (..))
import Downhill.VSpace (VSpace (..))
import NumHask.Prelude hiding (Field)
import Downhill.Tree.BVar (BVarField(..))

data MonoLeaf (a :: Type) :: () ~> Type
type instance Apply (MonoLeaf a) '() = a

instance (SingI sh, Additive a) => Additive (FieldWrapper (MonoLeaf a) sh) where
    zero = case sing @sh of
        STuple0 -> FieldWrapper zero
    FieldWrapper x + FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x + y)

instance (SingI sh, Subtractive a) => Subtractive (FieldWrapper (MonoLeaf a) sh) where
    negate = case sing @sh of
        STuple0 -> negate
    FieldWrapper x - FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x - y)

instance (SingI sh, Multiplicative a) => Multiplicative (FieldWrapper (MonoLeaf a) sh) where
    one = case sing @sh of
        STuple0 -> FieldWrapper one
    FieldWrapper x * FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x * y)

type Field f = f @@ '()

instance (Semigroup a, SingI sh) => Semigroup (FieldWrapper (MonoLeaf a) sh) where
    FieldWrapper x <> FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x <> y)
instance (Monoid a, SingI sh) => Monoid (FieldWrapper (MonoLeaf a) sh) where
    mempty = FieldWrapper $ case sing @sh of
        STuple0 -> mempty

instance (HasBuilder a, SingI sh) => HasBuilder (FieldWrapper (MonoLeaf a) sh) where
    type VecBuilder (FieldWrapper (MonoLeaf a) sh) = VecBuilder a
    sumBuilder = case sing @sh of
        STuple0 -> FieldWrapper . sumBuilder
    identityBuilder = case sing @sh of
        STuple0 -> identityBuilder . unFieldWrapper

instance (SingI sh) => Eq (FieldWrapper (MonoLeaf Int) sh) where
    FieldWrapper x == FieldWrapper y = case sing @sh of
        STuple0 -> x == y

instance (FromInteger a, SingI sh) => FromInteger (FieldWrapper (MonoLeaf a) sh) where
    fromInteger = case sing @sh of
        STuple0 -> FieldWrapper . fromInteger

type instance FieldTang (MonoLeaf a) = MonoLeaf (Tang a)
type instance FieldDualOf (MonoLeaf a) = MonoLeaf (DualOf a)
type instance FieldScalar (MonoLeaf a) = Scalar a

instance (MultiplicativeAction a, SingI sh) => MultiplicativeAction (FieldWrapper (MonoLeaf a) sh) where
    type Scalar (FieldWrapper (MonoLeaf a) sh) = Scalar a
    FieldWrapper v |* a = case sing @sh of
        STuple0 -> FieldWrapper (v |* a)

instance (VSpace a, SingI sh) => VSpace (FieldWrapper (MonoLeaf a) sh) where
    FieldWrapper dx |*| FieldWrapper x = case sing @sh of
        STuple0 -> dx |*| x

instance (Manifold a, SingI sh) => Manifold (FieldWrapper (MonoLeaf a) sh) where
    FieldWrapper x .+~^ FieldWrapper dx = case sing @sh of
        STuple0 -> FieldWrapper (x .+~^ dx)

splitMonoLeaf' :: forall r a. () => BVar r (FieldWrapper (MonoLeaf a) '()) -> FieldWrapper (MonoLeaf (BVar r a)) '()
splitMonoLeaf' (BVar (FieldWrapper x) dx) = FieldWrapper (BVar x (inlineNode unFieldWrapper id dx))

splitMonoLeaf :: forall r a sh. (SingI sh) => BVar r (FieldWrapper (MonoLeaf a) sh) -> FieldWrapper (MonoLeaf (BVar r a)) sh
splitMonoLeaf = case sing @sh of
    STuple0 -> splitMonoLeaf'

joinMonoLeaf' :: forall r a. () => FieldWrapper (MonoLeaf (BVar r a)) '() -> BVar r (FieldWrapper (MonoLeaf a) '())
joinMonoLeaf' (FieldWrapper (BVar x dx)) = BVar (FieldWrapper x) (inlineNode FieldWrapper id dx)

joinMonoLeaf :: forall r a sh. SingI sh => FieldWrapper (MonoLeaf (BVar r a)) sh -> BVar r (FieldWrapper (MonoLeaf a) sh)
joinMonoLeaf = case sing @sh of
    STuple0 -> joinMonoLeaf'

instance BVarField (MonoLeaf a) where
    type FieldBVar (r :: Type) (MonoLeaf a) = MonoLeaf (BVar r a)
    splitField = splitMonoLeaf
    joinField = joinMonoLeaf
