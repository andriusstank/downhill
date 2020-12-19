{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}

module Graph where
import ExprRef (ExprMap, ExprName, SomeExprWithName(..))
import Sharing(SharedTerm(..), SharedExpr(..), SharedArg(..))
import Tensor(TensorProduct(..), AFunction(..))

import qualified ExprRef as ExprMap
import Data.VectorSpace (sumV, AdditiveGroup)
import Data.Kind (Type)

class TensorProduct' x y where
    type ProductType x y :: Type

newtype WrapX x y xy = WrapX { unWrapX :: x }
newtype WrapY x y xy = WrapY { unWrapY :: y }

instance TensorProduct x y xy => TensorProduct' (WrapX x y xy) (WrapY x y xy) where
    type ProductType (WrapX x y xy) (WrapY x y xy) = xy

data f :⊗ g = f :⊗ g

instance (TensorProduct f x dv, TensorProduct g a x) => TensorProduct (f :⊗ g) a dv where
-- (f :⊗ g) ⊗ x = f ⊗ (g ⊗ x)

ff :: (TensorProduct f x w, TensorProduct g a x) => (f :⊗ g) -> a -> w
(f :⊗ g) `ff` x = f ⊗ (g ⊗ x)

data RightEndpoint b a da z dz v dv where
    RightVar :: RightEndpoint b a da z dz a da
    RightExpr :: SharedBiExpr b a da z dz v dv -> RightEndpoint b a da z dz v dv

data LeftEndpoint b a da z dz v dv where
    LeftVar :: LeftEndpoint b a da z dz z dz
    LeftExpr :: SharedBiExpr b a da z dz v dv -> LeftEndpoint b a da z dz v dv

data FwdEdge b a da z dz v dv where
    FwdEdge :: AFunction b u du v dv -> RightEndpoint b a da z dz u du -> FwdEdge b a da z dz v dv

data BackEdge b a da z dz u du where
    BackEdge :: LeftEndpoint b a da z dz v dv -> AFunction b u du v dv -> BackEdge b a da z dz u du

data InEdges b a da z dz v dv = (AdditiveGroup v, AdditiveGroup dv) => InEdges
    { shInEdges :: [FwdEdge b a da z dz v dv]
    }

data OutEdges b a da z dz v dv = (AdditiveGroup v, AdditiveGroup dv) => OutEdges
    { shOutEdges :: [BackEdge b a da z dz v dv]
    }

data SharedBiExpr b a da z dz v dv = SharedBiExpr (InEdges b a da z dz v dv) (OutEdges b a da z dz v dv)

instance TensorProduct (RightEndpoint b a da z dz v dv) a v where
    f ⊗ x = case f of
        RightVar -> x
        RightExpr f' -> _ -- f' ⊗ x

instance TensorProduct dz (LeftEndpoint b a da z dz v dv) dv where
    x ⊗ f = case f of
        LeftVar -> x
        LeftExpr f' -> _ --x ⊗ f'

instance TensorProduct (FwdEdge b a da z dz v dv) a v where
    FwdEdge f g ⊗ x = f ⊗ (g ⊗ x)

instance TensorProduct dz (BackEdge b a da z dz u du) du where
    dx ⊗ BackEdge f g = (dx ⊗ f) ⊗ g

instance TensorProduct (InEdges b a da z dz v dv) a v where
    e ⊗ x = case e of
        InEdges fs -> sumV [ f ⊗ x | f <- fs]

instance TensorProduct dz (OutEdges b a da z dz v dv) dv where
    x ⊗ e = case e of
        OutEdges fs -> sumV [x ⊗ f | f <- fs ]

--meetInTheMiddle :: TensorProduct dv v b => dz -> SharedBiExpr b a da z dz v dv -> a -> b
--meetInTheMiddle dz (SharedBiExpr f g) a = (dz ⊗ f) ⊗ (g ⊗ a)
