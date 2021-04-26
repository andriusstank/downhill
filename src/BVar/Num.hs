{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BVar.Num
where
import Affine (AffineFunc(AffineFunc))
import Data.Kind (Type)
import Data.VectorSpace (zeroV, AdditiveGroup(..), VectorSpace(..))
import Data.AffineSpace (AffineSpace(..))
import Expr (Expr, Expr (ExprVar), AnyExpr, anyVar)
import Notensor (FullVector(..), ProdVector(..), BasicVector(..), BackFun, NumBuilder (..))
import EType (Endpoint(SourceNode))
import Diff (backprop)

type family GradOf a :: Type

newtype AsNum a = AsNum { unAsNum :: a }
    deriving Show
    deriving Num via a
    deriving Fractional via a
    deriving Floating via a

instance Num a => AdditiveGroup (AsNum a) where
    zeroV = 0
    (^+^) = (+)
    (^-^) = (-)
    negateV = negate

instance Num a => VectorSpace (AsNum a) where
    type Scalar (AsNum a) = AsNum a
    (*^) = (*)

instance Num a => BasicVector (AsNum a) where
    type VecBuilder (AsNum a) = NumBuilder a
    sumBuilder = AsNum . sum . fmap unNumBuilder

instance Num a => ProdVector (AsNum a) where
    zeroBuilder = mempty
    identityBuilder = NumBuilder . unAsNum

instance Num a => FullVector (AsNum a) where
    negateBuilder = NumBuilder . negate . unAsNum
    scaleBuilder (AsNum x) (AsNum y) = NumBuilder $ x * y

instance Num a => AffineSpace (AsNum a) where
    type Diff (AsNum a) = AsNum a
    AsNum x .-. AsNum y = AsNum (x-y)
    AsNum x .+^ AsNum y = AsNum (x+y)

newtype NumBVar a = NumBVar (AffineFunc a (AnyExpr BackFun (AsNum a) (AsNum a)))
    deriving Num via (AffineFunc (AsNum a) (AnyExpr BackFun (AsNum a) (AsNum a)))
    deriving Fractional via (AffineFunc (AsNum a) (AnyExpr BackFun (AsNum a) (AsNum a)))
    deriving Floating via (AffineFunc (AsNum a) (AnyExpr BackFun (AsNum a) (AsNum a)))

constant :: Num a => a -> NumBVar a
constant x = NumBVar (AffineFunc x zeroV)

var :: Num a => a -> NumBVar a
var x = NumBVar (AffineFunc x anyVar)

backpropNum :: Num a => NumBVar a -> a
backpropNum (NumBVar x) = unAsNum $ backprop x 1

numbvarValue :: NumBVar a -> a
numbvarValue (NumBVar (AffineFunc y0 _dy)) = y0
