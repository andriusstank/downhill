{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# language ScopedTypeVariables #-}

{-# LANGUAGE DerivingVia #-}
module BVar.Num
where
import Affine (AffineFunc(AffineFunc))
import Data.Kind (Type)
import Data.VectorSpace (zeroV, AdditiveGroup(..), VectorSpace(..))
import Data.AffineSpace (AffineSpace(..))
import Expr (LinearFunc5(LinearFunc5))
import Notensor (FullVector(..), ProdVector(..), BasicVector(..), BackFunc)
import EType (Endpoint(SourceNode))
import Diff (backprop)

type family GradOf a :: Type

newtype AsNum a = AsNum { unAsNum :: a }
    deriving Num via a

instance Num a => AdditiveGroup (AsNum a) where
    zeroV = AsNum 0
    negateV (AsNum x) = AsNum (negate x)
    AsNum x ^+^ AsNum y = AsNum (x+y)
    AsNum x ^-^ AsNum y = AsNum (x-y)

instance Num a => VectorSpace (AsNum a) where
    type Scalar (AsNum a) = AsNum a
    AsNum a *^ AsNum v = AsNum (a*v)

instance Num a => AffineSpace (AsNum a) where
    type Diff (AsNum a) = AsNum a
    AsNum x .-. AsNum y = AsNum (x-y)
    AsNum x .+^ AsNum y = AsNum (x+y)

instance Num a => BasicVector (AsNum a) where
    type VecBuilder (AsNum a) = AsNum a
    sumBuilder = sum

instance Num a => ProdVector (AsNum a) where
    zeroBuilder = 0
    identityBuilder = Prelude.id

instance Num a => FullVector (AsNum a) where
    negateBuilder = negate
    scaleBuilder = (*)

newtype NumBVar a = NumBVar (AffineFunc a (LinearFunc5 BackFunc (AsNum a) (AsNum a)))
    deriving Num via (AffineFunc (AsNum a) (LinearFunc5 BackFunc (AsNum a) (AsNum a)))

constant :: Num a => a -> NumBVar a
constant x = NumBVar (AffineFunc x zeroV)

var :: Num a => a -> NumBVar a
var x = NumBVar (AffineFunc x (LinearFunc5 SourceNode))

backpropNum :: Num a => NumBVar a -> a
backpropNum (NumBVar x) = unAsNum $ backprop x 1

numbvarValue :: NumBVar a -> a
numbvarValue (NumBVar (AffineFunc y0 _dy)) = y0
