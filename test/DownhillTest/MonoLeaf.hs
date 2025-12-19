{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DownhillTest.MonoLeaf(MonoLeaf, Field)
where
import Data.Singletons (Apply, SingI (sing), type (@@), type (~>))
import Data.Tuple.Singletons (STuple0 (STuple0))
import Downhill.Tree.FieldWrapper (FieldWrapper (..), DenseFieldWrapper(..))
import NumHask.Prelude hiding (Field)
import Downhill.Linear.Builder (HasBuilder)

data MonoLeaf (a :: Type) :: () ~> Type
type instance Apply (MonoLeaf a) '() = a

instance (SingI sh, Additive a) => Additive (FieldWrapper (MonoLeaf a) sh) where
    zero = case sing @sh of
        STuple0 -> FieldWrapper zero
    FieldWrapper x + FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x + y)

instance (SingI sh, Multiplicative a) => Multiplicative (FieldWrapper (MonoLeaf a) sh) where
    one = case sing @sh of
        STuple0 -> FieldWrapper one
    FieldWrapper x * FieldWrapper y = case sing @sh of
        STuple0 -> FieldWrapper (x * y)

type Field f = f @@ '()

deriving via (DenseFieldWrapper (MonoLeaf a) sh)
    instance
        (Additive a, SingI sh) => HasBuilder (FieldWrapper (MonoLeaf a) sh)


instance SingI sh => Eq (FieldWrapper (MonoLeaf Int) sh) where
    FieldWrapper x == FieldWrapper y = case sing @sh of
        STuple0 -> x==y

instance (FromInteger a, SingI sh) => FromInteger (FieldWrapper (MonoLeaf a) sh) where
    fromInteger = case sing @sh of
        STuple0 -> FieldWrapper . fromInteger
