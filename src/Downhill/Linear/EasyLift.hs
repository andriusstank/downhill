{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Downhill.Linear.EasyLift
( lift1, lift2, lift3
)
where
import Downhill.Linear.Expr
    ( Expr(ExprSum),
      Term(..),
      BasicVector(..),
      BackFun(..) )
import Prelude hiding (fst, snd, zip)
import Data.Reflection (Reifies(reflect), reify)
import Data.Proxy (Proxy(Proxy))
import Downhill.Linear.BackGrad(BackGrad(..), HasGrad (GradOf), GradBuilder, castNode)

newtype Fun1 a z = Fun1 (GradOf z -> GradBuilder a)

newtype BuilderTuple1 s a z = BuilderTuple1 (GradBuilder a)

instance (Reifies s (Fun1 a z), BasicVector (GradOf z)) => BasicVector (BuilderTuple1 s a z) where
    type VecBuilder (BuilderTuple1 s a z) = GradBuilder z
    sumBuilder zbs = wrap (f z)
        where z = sumBuilder zbs :: GradOf z
              wrap a = BuilderTuple1 a
              Fun1 f = reflect (Proxy @s):: Fun1 a z

go1 :: forall s r a z. (Reifies s (Fun1 a z), BasicVector (GradOf z)) => BackGrad r a -> Proxy s -> BackGrad r z
go1 (BackGrad g1)Proxy = castNode node
    where t1 :: [Term BackFun (GradOf r) (BuilderTuple1 s a z)]
          t1 = g1 (\(BuilderTuple1 x) -> x)
          node :: Expr BackFun (GradOf r) (BuilderTuple1 s a z)
          node = ExprSum t1

lift1 :: forall r a z. BasicVector (GradOf z) => (GradOf z -> GradBuilder a) -> BackGrad r a -> BackGrad r z
lift1 f x = reify (Fun1 f) (go1 x)



newtype Fun2 a b z = Fun2 (GradOf z -> (GradBuilder a, GradBuilder b))

data BuilderTuple2 s a b z = BuilderTuple2 (GradBuilder a) (GradBuilder b)

instance (Reifies s (Fun2 a b z), BasicVector (GradOf z)) => BasicVector (BuilderTuple2 s a b z) where
    type VecBuilder (BuilderTuple2 s a b z) = GradBuilder z
    sumBuilder zbs = wrap (f z)
        where z = sumBuilder zbs :: GradOf z
              wrap (a, b) = BuilderTuple2 a b
              Fun2 f = reflect (Proxy @s):: Fun2 a b z

go2 :: forall s r a b z. (Reifies s (Fun2 a b z), BasicVector (GradOf z)) => BackGrad r a -> BackGrad r b -> Proxy s -> BackGrad r z
go2 (BackGrad g1) (BackGrad g2) Proxy = castNode node
    where t1, t2 :: [Term BackFun (GradOf r) (BuilderTuple2 s a b z)]
          t1 = g1 (\(BuilderTuple2 x _) -> x)
          t2 = g2 (\(BuilderTuple2 _ y) -> y)
          node :: Expr BackFun (GradOf r) (BuilderTuple2 s a b z)
          node = ExprSum (t1 ++ t2)

lift2 :: forall r a b z. BasicVector (GradOf z) => (GradOf z -> (GradBuilder a, GradBuilder b)) -> BackGrad r a -> BackGrad r b -> BackGrad r z
lift2 f x y = reify (Fun2 f) (go2 x y)



newtype Fun3 a b c z = Fun3 (GradOf z -> (GradBuilder a, GradBuilder b, GradBuilder c))

data BuilderTuple3 s a b c z = BuilderTuple3 (GradBuilder a) (GradBuilder b) (GradBuilder c)

instance (Reifies s (Fun3 a b c z), BasicVector (GradOf z)) => BasicVector (BuilderTuple3 s a b c z) where
    type VecBuilder (BuilderTuple3 s a b c z) = GradBuilder z
    sumBuilder zbs = wrap (f z)
        where z = sumBuilder zbs :: GradOf z
              wrap (a, b, c) = BuilderTuple3 a b c
              Fun3 f = reflect (Proxy @s):: Fun3 a b c z

go3 :: forall s r a b c z. (Reifies s (Fun3 a b c z), BasicVector (GradOf z)) => BackGrad r a -> BackGrad r b -> BackGrad r c -> Proxy s -> BackGrad r z
go3 (BackGrad g1) (BackGrad g2) (BackGrad g3) Proxy = castNode node
    where t1, t2, t3 :: [Term BackFun (GradOf r) (BuilderTuple3 s a b c z)]
          t1 = g1 (\(BuilderTuple3 x _ _) -> x)
          t2 = g2 (\(BuilderTuple3 _ y _) -> y)
          t3 = g3 (\(BuilderTuple3 _ _ z) -> z)
          node :: Expr BackFun (GradOf r) (BuilderTuple3 s a b c z)
          node = ExprSum (t1 ++ t2 ++ t3)

lift3
    :: forall r a b c z. BasicVector (GradOf z)
    => (GradOf z -> (GradBuilder a, GradBuilder b, GradBuilder c))
    -> BackGrad r a -> BackGrad r b -> BackGrad r c
    -> BackGrad r z
lift3 f x y z = reify (Fun3 f) (go3 x y z)
