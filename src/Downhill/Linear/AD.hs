{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Downhill.Linear.AD (
  AD (..),
  Graph,
  ad,
  adDense,
  identityAD,
  SparseFun (..),
  realNode,
  realNode',
  realNode2,
  sparseNode,
  inlineNode,
  dense1,
  dense2,
  pattern T2,
  sparseTerm,
  denseTerm',
)
where

import Downhill.Internal.Graph.Graph (DenseBigraph (DenseBigraph), Graph, buildBigraph)
import Downhill.Linear.Builder (Dense, HasBuilder (..), Sparse (unSparse))
import Downhill.Linear.Expr (BiFun (BiFun), Expr (ExprSum, ExprVar), GradType, TangType, Term (Term), VecPair (VecPair), idBiFun)
import Downhill.Linear.Fun (DualOf, IdFun (IdFun), LinearMap (..), (<*|), (|*>), NegateFun (NegateFun), ScalarMulFun (ScalarMulFun))
import Downhill.VSpace (VSpace)
import NumHask.Prelude

newtype AD (a :: Type) (v :: Type)
  = AD
      ( forall x.
        BiFun ('VecPair v (DualOf v)) x ->
        Term ('VecPair a (DualOf a)) x
      )

identityAD :: AD a a
identityAD = realNode ExprVar

realNode :: Expr ('VecPair a (DualOf a)) ('VecPair v (DualOf v)) -> AD a v
realNode x = AD (\(BiFun f g) -> Term (BiFun f g) x)

realNode2 :: forall a v. ( HasBuilder v, HasBuilder (DualOf v)) => [Term ('VecPair a (DualOf a)) ('VecPair v (DualOf v))] -> AD a v
realNode2 = realNode . ExprSum

realNode' ::
  (VecBuilder (DualOf v) ~ VecBuilder dv') =>
  Expr ('VecPair a (DualOf a)) ('VecPair v dv') -> AD a v
realNode' y = AD (\(BiFun f g) -> Term (BiFun f g) y)

denseTerm :: forall r f u v.
  ( LinearMap f u v
  , HasBuilder v
  , HasBuilder (DualOf u)
  , DualOf (DualOf v) ~ v
  , DualOf (DualOf u) ~ u
  ) =>
  f -> AD r u -> Term ('VecPair r (DualOf r)) ('VecPair v (DualOf v))
denseTerm f (AD x) = x $ BiFun (identityBuilder . (f <*|)) (identityBuilder . (|*> f))

denseTerm' ::
  ( HasBuilder v
  , HasBuilder (DualOf u)
  ) =>
  (u -> v) -> (DualOf v -> DualOf u) -> AD r u -> Term ('VecPair r (DualOf r)) ('VecPair v (DualOf v))
denseTerm' f ft (AD x) = x $ BiFun (identityBuilder . f) (identityBuilder . ft)

dense1 ::
  ( LinearMap f u v
  , HasBuilder v
  , HasBuilder (DualOf v)
  , HasBuilder (DualOf u)
  , DualOf (DualOf u) ~ u
  , DualOf (DualOf v) ~ v
  ) =>
  f ->
  AD r u ->
  AD r v
dense1 f1 x =
  realNode
    ( ExprSum
        [ denseTerm f1 x
        ]
    )

dense2 ::
  ( LinearMap f1 u1 v
  , LinearMap f2 u2 v
  , HasBuilder v
  , HasBuilder (DualOf v)
  , HasBuilder (DualOf u1)
  , HasBuilder (DualOf u2)
  , DualOf (DualOf u1) ~ u1
  , DualOf (DualOf u2) ~ u2
  , DualOf (DualOf v) ~ v
  ) =>
  f1 ->
  f2 ->
  AD r u1 ->
  AD r u2 ->
  AD r v
dense2 f1 f2 x y =
  realNode
    ( ExprSum
        [ denseTerm f1 x
        , denseTerm f2 y
        ]
    )

instance (HasBuilder v, HasBuilder (DualOf v), v ~ DualOf (DualOf v)) => Additive (AD r v) where
  zero = realNode (ExprSum [])
  (+) = dense2 IdFun IdFun

instance (HasBuilder v, HasBuilder (DualOf v), Subtractive v, Subtractive (DualOf v), v ~ DualOf (DualOf v)) => Subtractive (AD r v) where
  negate = dense1 NegateFun
  (-) = dense2 IdFun NegateFun

instance
  ( HasBuilder v
  , HasBuilder (DualOf v)
  , VSpace v
  ) =>
  MultiplicativeAction (AD r v)
  where
  type Scalar (AD r v) = Scalar v
  v |* x = dense1 (ScalarMulFun x) v

-- TODO: remove SparseFun?
data SparseFun (u :: VecPair) (v :: VecPair)
  = SparseFun
      (TangType u -> VecBuilder (TangType v))
      (VecBuilder (GradType v) -> VecBuilder (GradType u))

sparseTerm ::
  (HasBuilder v, HasBuilder u, du ~ DualOf u, dv ~ DualOf v) =>
  SparseFun ('VecPair u du) ('VecPair v dv) ->
  AD r u ->
  Term ('VecPair r (DualOf r)) ('VecPair v (Sparse dv))
sparseTerm (SparseFun f1 g1) (AD df) = df $ BiFun f1 (g1 . unSparse)

sparseNode ::
  forall r u du v dv.
  (HasBuilder v, HasBuilder u, HasBuilder dv, du ~ DualOf u, dv ~ DualOf v) =>
  SparseFun ('VecPair u du) ('VecPair v dv) ->
  AD r u ->
  AD r v
sparseNode f x = realNode' (ExprSum [sparseTerm f x])

inlineNode ::
  forall r u v.
  (u -> v) ->
  (VecBuilder (DualOf v) -> VecBuilder (DualOf u)) ->
  AD r u ->
  AD r v
inlineNode f1 g1 (AD g) = AD go
 where
  go ::
    forall x.
    BiFun ('VecPair v (DualOf v)) x ->
    Term ('VecPair r (DualOf r)) x
  go (BiFun f2 g2) = g $ BiFun (f2 . f1) (g1 . g2)

maybeToMonoid :: (Monoid m) => Maybe m -> m
maybeToMonoid = fromMaybe mempty

fstFun :: forall a da b db. (HasBuilder a, HasBuilder b, HasBuilder db) => SparseFun ('VecPair (a, b) (da, db)) ('VecPair a da)
fstFun = SparseFun fwd back
 where
  fwd :: (a, b) -> VecBuilder a
  fwd (x, _y) = identityBuilder x
  back :: VecBuilder da -> (Maybe (Sum (da, db)), Maybe (VecBuilder da, VecBuilder db))
  back x = (Nothing, Just (x, mempty))

sndFun :: forall a da b db. (HasBuilder a, HasBuilder b, HasBuilder da) => SparseFun ('VecPair (a, b) (da, db)) ('VecPair b db)
sndFun = SparseFun fwd back
 where
  fwd :: (a, b) -> VecBuilder b
  fwd (_x, y) = identityBuilder y
  back :: VecBuilder db -> (Maybe (Sum (da, db)), Maybe (VecBuilder da, VecBuilder db))
  back x = (Nothing, Just (mempty, x))

splitPair ::
  forall r a da b db.
  (HasBuilder a, HasBuilder b, HasBuilder db, HasBuilder da, da ~ DualOf a, db ~ DualOf b) =>
  AD r (a, b) ->
  (AD r a, AD r b)
splitPair x = (sparseNode fstFun x, sparseNode sndFun x)

intoFstFun :: forall a da b db. (HasBuilder a, HasBuilder b, HasBuilder db, HasBuilder da) => SparseFun ('VecPair da a) ('VecPair (da, db) (a, b))
intoFstFun = SparseFun back fwd
 where
  fwd :: VecBuilder (a, b) -> VecBuilder a
  fwd (x, y) = maybeToMonoid (xPart <> (fst <$> y))
   where
    xPart :: Maybe (VecBuilder a)
    xPart = identityBuilder . fst . getSum <$> x
  back :: da -> (Maybe (Sum (da, db)), Maybe (VecBuilder da, VecBuilder db))
  back x = (Nothing, Just (identityBuilder x, mempty))

intoSndFun :: forall a da b db. (HasBuilder a, HasBuilder b, HasBuilder da, HasBuilder db) => SparseFun ('VecPair db b) ('VecPair (da, db) (a, b))
intoSndFun = SparseFun back fwd
 where
  fwd :: VecBuilder (a, b) -> VecBuilder b
  fwd (x, y) = maybeToMonoid (xPart <> (snd <$> y))
   where
    xPart :: Maybe (VecBuilder b)
    xPart = identityBuilder . snd . getSum <$> x
  back :: db -> (Maybe (Sum (da, db)), Maybe (VecBuilder da, VecBuilder db))
  back x = (Nothing, Just (mempty, identityBuilder x))

joinPair ::
  forall r a da b db.
  (HasBuilder a, HasBuilder b, HasBuilder da, HasBuilder db, db ~ DualOf b, da ~ DualOf a) =>
  (AD r a, AD r b) -> AD r (a, b)
joinPair (x, y) = realNode' (ExprSum [t1, t2])
 where
  t1, t2 :: Term ('VecPair r (DualOf r)) ('VecPair (a, b) (Sparse (da, db)))
  t1 = sparseTerm intoFstFun x
  t2 = sparseTerm intoSndFun y

{-# COMPLETE T2 #-}
pattern T2 ::
  forall r a da b db.
  (HasBuilder a, HasBuilder b, HasBuilder da, HasBuilder db, da ~ DualOf a, db ~ DualOf b) =>
  AD r a -> AD r b -> AD r (a, b)
pattern T2 a b <-
  (splitPair -> (a, b))
  where
    T2 a b = joinPair (a, b)

ad ::
  forall a z.
  ( HasBuilder a
  , HasBuilder z
  , HasBuilder (DualOf z)
  , HasBuilder (DualOf a)
  ) =>
  AD a z -> Graph a z
ad (AD f) = buildBigraph [f idBiFun]

adDense ::
  (Additive a, Additive z, Additive (DualOf z), Additive (DualOf a)) =>
  AD (Dense a) (Dense z) -> DenseBigraph a z
adDense = DenseBigraph . ad