{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BVar.Vec
where
import Prelude hiding (id)
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import Data.Kind (Type)
import Affine (AffineFunc(AffineFunc))
import Tensor (Bilinear(..))
import Expr (Expr(..))
import Notensor (BackFunc(BackFunc), FullVector, LinearEdge (scaleFunc), BasicVector (VecBuilder))
import Control.Category (Category(..))
import EType (Endpoint(InnerNode), Node(..), Edge(..))

type family GradOf a :: Type

newtype VecBVar a v = VecBVar (AffineFunc v (Expr BackFunc a (GradOf v)))

deriving via (AffineFunc v (Expr BackFunc a (GradOf v))) instance (AdditiveGroup v, FullVector (GradOf v)) => AdditiveGroup (VecBVar a v)

{-
foo :: dv -> v -> VecBuilder (GradOf (Scalar dv))
foo dv v = undefined -- dv ✕ v

instance (dv ~ GradOf v, VectorSpace v, FullVector dv, Scalar v ~ Scalar dv, Bilinear dv v) => VectorSpace (VecBVar a v) where
    type Scalar (VecBVar a v) = VecBVar a (Scalar v)
    VecBVar (AffineFunc a0 (LinearFunc5 da)) *^ VecBVar (AffineFunc v0 (LinearFunc5 dv)) = VecBVar (AffineFunc (a0 *^ v0) d)
        where --d1 = a0 *^ dv :: LinearFunc5 BackFunc a (GradOf v)
              --d2 = _ da v0
              d = LinearFunc5 (InnerNode (Expr5 (Node [e1, e2])))
                where e1, e2 :: Edge (Expr5 BackFunc a) BackFunc a dv
                      e1 = Edge (scaleFunc @BackFunc a0) dv
                      e2 = Edge (BackFunc (\dv -> foo dv v0)) da -- ???
-}


constant :: (dv ~ GradOf v, FullVector dv, AdditiveGroup dv) => v -> VecBVar a v
constant x = VecBVar (AffineFunc x zeroV)

var :: dv ~ GradOf v => v -> VecBVar dv v
var x = VecBVar (AffineFunc x ExprVar)


{-
instance AdditiveGroup (VecBVar a) where
    (^+^) = undefined
    (^-^) = undefined
    zeroV = undefined
    negateV = undefined

instance
    ( VectorSpace (GradOf a)
    , VectorSpace a
    , Scalar (GradOf a) ~ Scalar a
    , GradOf a ~ (GradOf (Scalar a) ✕ a)
    , Bilinear (GradOf (Scalar a)) a
    ) => VectorSpace (VecBVar a) where
    type Scalar (VecBVar a) = VecBVar (Scalar a)
    BVar (AffineFunc a da) *^ BVar (AffineFunc v dv) = BVar (AffineFunc (a*^v) (part1 ^+^ part2))
        where part1 :: GradOf a
              part1 = a *^ dv
              part2 :: GradOf a
              part2 = da ✕ v
-}
