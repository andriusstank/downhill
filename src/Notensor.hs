{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Notensor
( FullVector(..), Dense(..)

) where
import Data.VectorSpace (AdditiveGroup(..), VectorSpace(..))
import Downhill.Linear.Expr(BasicVector(..), FullVector(..))

newtype Dense a = Dense a
    deriving AdditiveGroup via a
    deriving VectorSpace via a

newtype VSpaceBuilder a = VSpaceBuilder { unVSpaceBuilder :: a }

instance AdditiveGroup a => Semigroup (VSpaceBuilder a) where
    VSpaceBuilder x <> VSpaceBuilder y = VSpaceBuilder (x ^+^ y)

instance AdditiveGroup a => Monoid (VSpaceBuilder a) where
    mempty = VSpaceBuilder zeroV

instance AdditiveGroup a => BasicVector (Dense a) where
    type VecBuilder (Dense a) = VSpaceBuilder a
    sumBuilder = Dense . unVSpaceBuilder

instance VectorSpace a => FullVector (Dense a) where
    identityBuilder (Dense x) = VSpaceBuilder x
    negateBuilder (Dense a) = VSpaceBuilder (negateV a)
    scaleBuilder a (Dense v) = VSpaceBuilder (a *^ v)


