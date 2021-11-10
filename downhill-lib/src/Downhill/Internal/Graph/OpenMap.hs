{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Downhill.Internal.Graph.OpenMap
  ( OpenMap,
    OpenKey,
    SomeOpenItem (SomeOpenItem),
    map,
    mapWithKey,
    mapMaybe,
    lookup,
    toList,
    elems,
    empty,
    insert,
    adjust,
    intersectionWith,
    makeOpenKey,
  )
where

import Control.Applicative (Const (Const))
import Control.Exception (evaluate)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Kind (Type)
import GHC.Base (Any, Maybe (Just, Nothing), coerce)
import GHC.StableName (StableName)
import System.Mem.StableName (makeStableName)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Functor (fmap), IO, Monad (return), (.), (<$>))

data SomeExpr f = forall v. SomeExpr (f v)

-- | A key of @OpenMap@.
newtype OpenKey x = OpenKey (StableName Any)

-- | Heterogeneous map with 'StableName' as a key.
newtype OpenMap (f :: Type -> Type) = OpenMap {unOpenMap :: HashMap (StableName Any) (SomeExpr f)}

-- | Key and value.
data SomeOpenItem f = forall x. SomeOpenItem (OpenKey x) (f x)

empty :: OpenMap f
empty = OpenMap HashMap.empty

map :: forall f g. (forall dv. f dv -> g dv) -> OpenMap f -> OpenMap g
map f = OpenMap . fmap go . unOpenMap
  where
    go (SomeExpr y) = SomeExpr (f y)

mapMaybe :: forall f g. (forall dv. f dv -> Maybe (g dv)) -> OpenMap f -> OpenMap g
mapMaybe f = OpenMap . HashMap.mapMaybe go . unOpenMap
  where
    go (SomeExpr y) = case f y of
      Just fy -> Just (SomeExpr fy)
      Nothing -> Nothing

mapWithKey :: forall f g. (forall d. OpenKey d -> f d -> g d) -> OpenMap f -> OpenMap g
mapWithKey f = OpenMap . HashMap.mapWithKey go . unOpenMap
  where
    go key (SomeExpr y) = SomeExpr (f (OpenKey key) y)

lookup :: OpenMap f -> OpenKey x -> Maybe (f x)
lookup (OpenMap m) (OpenKey k) = unsafeCastTypeSomeExpr <$> HashMap.lookup k m

toList :: OpenMap f -> [SomeOpenItem f]
toList = fmap wrap . HashMap.toList . unOpenMap
  where
    wrap :: (StableName Any, SomeExpr f) -> SomeOpenItem f
    wrap (key, x) = case x of
      SomeExpr x' -> SomeOpenItem (OpenKey key) x'

elems :: OpenMap (Const b) -> [b]
elems = fmap unSomeExpr . HashMap.elems . unOpenMap
  where
    unSomeExpr :: SomeExpr (Const r) -> r
    unSomeExpr (SomeExpr (Const x)) = x

unsafeCastTypeSomeExpr :: SomeExpr f -> f v
unsafeCastTypeSomeExpr = \case
  SomeExpr x -> unsafeCoerce x

intersectionWith :: forall f g h. (forall dx. f dx -> g dx -> h dx) -> OpenMap f -> OpenMap g -> OpenMap h
intersectionWith f (OpenMap x) (OpenMap y) = OpenMap (HashMap.intersectionWith f' x y)
  where
    f' (SomeExpr x') sy = SomeExpr (f x' y')
      where
        y' = unsafeCastTypeSomeExpr sy

insert :: forall f dx. OpenKey dx -> f dx -> OpenMap f -> OpenMap f
insert (OpenKey k) x (OpenMap m) = OpenMap (HashMap.insert k (SomeExpr x) m)

adjust :: forall f dx. (f dx -> f dx) -> OpenKey dx -> OpenMap f -> OpenMap f
adjust f (OpenKey key) (OpenMap m) = OpenMap m'
  where
    m' = HashMap.adjust f' key m
    f' x = SomeExpr (f (unsafeCastTypeSomeExpr x))

makeOpenKey :: f v -> IO (OpenKey v)
makeOpenKey x = do
  x' <- evaluate x
  z <- makeStableName x'
  return (OpenKey (coerce z))
