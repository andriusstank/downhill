{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Algebra(FrobeniusAlgebra(..))
where
import Downhill.VSpace (VSpace, RealScalar)
import Downhill.Linear.Fun (DualOf)
import NumHask.Prelude

-- | Symmetric Frobenius algebra
-- `frobenius x |*| y = trace (x ^*^ y)`

class (VSpace a, Multiplicative a) => FrobeniusAlgebra a where
    frobenius :: a -> DualOf a
    cofrobenius :: DualOf a -> a
    trace :: a -> Scalar a

instance FrobeniusAlgebra Integer where
    frobenius = id
    cofrobenius = id
    trace = id

instance FrobeniusAlgebra Int where
    frobenius = id
    cofrobenius = id
    trace = id

instance RealScalar a => FrobeniusAlgebra (Complex a) where
    frobenius = id
    cofrobenius = id
    trace = id

instance FrobeniusAlgebra Float where
    frobenius = id
    cofrobenius = id
    trace = id

instance FrobeniusAlgebra Double where
    frobenius = id
    cofrobenius = id
    trace = id
