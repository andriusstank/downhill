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
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Graph where
import ExprRef (ExprMap, ExprName, SomeExprWithName(..))
import Sharing(SharedTerm(..), SharedExpr(..), SharedArg(..))
import Tensor(LinearFunction, TensorProduct(..), AFunction(..))

import qualified ExprRef as ExprMap
import Data.VectorSpace (sumV, AdditiveGroup)
import Data.Kind (Type)

data f :⊗ g :: Type -> Type -> Type -> Type -> Type where
    (:⊗) :: (LinearFunction (f x v dx dv) x v dx dv, LinearFunction (g u x du dx) u x du dx) => f x v dx dv -> g u x du dx -> (f :⊗ g) u v du dv

instance TensorProduct ((f :⊗ g) u v du dv) u v where
    (f :⊗ g) ⊗ x = f ⊗ (g ⊗ x)

instance TensorProduct dv ((f :⊗ g) u v du dv) du where
    x ⊗ (f :⊗ g) = (x ⊗ f) ⊗ g

instance
  ( AdditiveGroup u
  , AdditiveGroup v
  , AdditiveGroup du
  , AdditiveGroup dv
  ) => LinearFunction ((f :⊗ g) u v du dv) u v du dv where
    
--instance (TensorProduct f x dv, TensorProduct g a x) => TensorProduct (f :⊗ g) a dv where
-- (f :⊗ g) ⊗ x = f ⊗ (g ⊗ x)

--ff :: (TensorProduct f x w, TensorProduct g a x) => (f :⊗ g) -> a -> w
--(f :⊗ g) `ff` x = f ⊗ (g ⊗ x)

{-
data RightEndpoint b a da z dz v dv where
    RightVar :: RightEndpoint b a da z dz a da
    RightExpr :: SharedBiNode b a da z dz v dv -> RightEndpoint b a da z dz v dv

data LeftEndpoint b a da z dz v dv where
    LeftVar :: LeftEndpoint b a da z dz z dz
    LeftExpr :: SharedBiNode b a da z dz v dv -> LeftEndpoint b a da z dz v dv

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
-}

--meetInTheMiddle :: TensorProduct dv v b => dz -> SharedBiExpr b a da z dz v dv -> a -> b
--meetInTheMiddle dz (SharedBiExpr f g) a = (dz ⊗ f) ⊗ (g ⊗ a)
