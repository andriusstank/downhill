{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DownhillTest.MonoLeaf (MonoLeaf, Field)
where

import Data.Constraint (Dict (Dict))
import Data.Singletons (Apply, SingI (sing), type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.BVar (BVar (..))
import Downhill.Linear.AD (inlineNode)
import Downhill.Linear.Builder (HasBuilder (identityBuilder), VecBuilder, sumBuilder)
import Downhill.Linear.Fun (DualOf)
import Downhill.Manifold (Manifold (..), Tang)
import Downhill.Tree.At (At (..), At, pattern At, unAt)
import Downhill.VSpace (VSpace (..))
import NumHask.Prelude hiding (Field)
import Downhill.Tree.BVar (BVarField(..), BVarRepr (..))
import Downhill.Tree.Representation (AllConstrained (..), PointwiseManifold (..), PointwiseVSpace (..), ReprTang, ReprDualOf, ReprScalar)

data MonoLeaf (a :: Type) :: () ~> Type
type instance Apply (MonoLeaf a) '() = a

instance (SingI sh, Additive a) => Additive (At (MonoLeaf a) sh) where
    zero = case sing @sh of
        STuple0 -> At zero
    At x + At y = case sing @sh of
        STuple0 -> At (x + y)

instance (SingI sh, Subtractive a) => Subtractive (At (MonoLeaf a) sh) where
    negate = case sing @sh of
        STuple0 -> negate
    At x - At y = case sing @sh of
        STuple0 -> At (x - y)

instance (SingI sh, Multiplicative a) => Multiplicative (At (MonoLeaf a) sh) where
    one = case sing @sh of
        STuple0 -> At one
    At x * At y = case sing @sh of
        STuple0 -> At (x * y)

type Field f = f @@ '()

instance (Semigroup a, SingI sh) => Semigroup (At (MonoLeaf a) sh) where
    At x <> At y = case sing @sh of
        STuple0 -> At (x <> y)
instance (Monoid a, SingI sh) => Monoid (At (MonoLeaf a) sh) where
    mempty = At $ case sing @sh of
        STuple0 -> mempty

instance (HasBuilder a, SingI sh) => HasBuilder (At (MonoLeaf a) sh) where
    type VecBuilder (At (MonoLeaf a) sh) = VecBuilder a
    sumBuilder = case sing @sh of
        STuple0 -> At . sumBuilder
    identityBuilder = case sing @sh of
        STuple0 -> identityBuilder . unAt

instance (SingI sh) => Eq (At (MonoLeaf Int) sh) where
    At x == At y = case sing @sh of
        STuple0 -> x == y

instance (FromInteger a, SingI sh) => FromInteger (At (MonoLeaf a) sh) where
    fromInteger = case sing @sh of
        STuple0 -> At . fromInteger

type instance ReprTang (MonoLeaf a) = MonoLeaf (Tang a)
type instance ReprDualOf (MonoLeaf a) = MonoLeaf (DualOf a)
type instance ReprScalar (MonoLeaf a) = Scalar a

instance (c a) => AllConstrained c (MonoLeaf a) where
    allConstrained STuple0 = Dict

instance (VSpace a) => PointwiseVSpace (MonoLeaf a) where
    vspaceDict STuple0 = Dict

instance (Manifold a) => PointwiseManifold (MonoLeaf a) where
    manifoldDict STuple0 = Dict

instance (MultiplicativeAction a, SingI sh) => MultiplicativeAction (At (MonoLeaf a) sh) where
    type Scalar (At (MonoLeaf a) sh) = Scalar a
    At v |* a = case sing @sh of
        STuple0 -> At (v |* a)

instance (VSpace a, SingI sh) => VSpace (At (MonoLeaf a) sh) where
    At dx |*| At x = case sing @sh of
        STuple0 -> dx |*| x

instance (Manifold a, SingI sh) => Manifold (At (MonoLeaf a) sh) where
    At x .+~^ At dx = case sing @sh of
        STuple0 -> At (x .+~^ dx)
    At x .-~^ At dx = case sing @sh of
        STuple0 -> At (x .-~^ dx)

splitMonoLeaf' :: forall r a. () => BVar r (At (MonoLeaf a) '()) -> At (MonoLeaf (BVar r a)) '()
splitMonoLeaf' (BVar (At x) dx) = At (BVar x (inlineNode unAt id dx))

splitMonoLeaf :: forall r a sh. (SingI sh) => BVar r (At (MonoLeaf a) sh) -> At (MonoLeaf (BVar r a)) sh
splitMonoLeaf = case sing @sh of
    STuple0 -> splitMonoLeaf'

joinMonoLeaf' :: forall r a. () => At (MonoLeaf (BVar r a)) '() -> BVar r (At (MonoLeaf a) '())
joinMonoLeaf' (At (BVar x dx)) = BVar (At x) (inlineNode At id dx)

joinMonoLeaf :: forall r a sh. SingI sh => At (MonoLeaf (BVar r a)) sh -> BVar r (At (MonoLeaf a) sh)
joinMonoLeaf = case sing @sh of
    STuple0 -> joinMonoLeaf'

instance BVarField (MonoLeaf a) where
    type FieldBVar (r :: Type) (MonoLeaf a) = MonoLeaf (BVar r a)
    splitField = splitMonoLeaf
    joinField = joinMonoLeaf

instance BVarRepr (MonoLeaf a) where
    type ReprBVar r (MonoLeaf a) = MonoLeaf (BVar r a)
    splitBVarField STuple0 = id
    joinBVarField STuple0 = id
