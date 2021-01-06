{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module OpenMap
    ( OpenKey(..), OpenMap(..)
    , SomeOpenItem(SomeOpenItem)
    , mapmap, mapmapWithKey, lookup, toList
    , empty, insert
    , intersectionWith
)
where

import Prelude ((.), Functor(fmap), (<$>), Maybe)
import GHC.StableName (StableName)
import Data.HashMap.Lazy (HashMap)
import GHC.Base (Any)
import Types (SomeExpr(SomeExpr))
import qualified Data.HashMap.Lazy as HashMap
import Unsafe.Coerce (unsafeCoerce)

newtype OpenKey x dx = OpenKey (StableName Any)
newtype OpenMap f = OpenMap { unOpenMap :: HashMap (StableName Any) (SomeExpr f) }

data SomeOpenItem f = forall x dx. SomeOpenItem (OpenKey x dx) (f x dx)

mapmap :: forall f g. (forall v dv. f v dv -> g v dv) -> OpenMap f -> OpenMap g
mapmap f = OpenMap . fmap go . unOpenMap
    where go (SomeExpr y) = SomeExpr (f y)

mapmapWithKey :: forall f g. (forall x dx. OpenKey x dx -> f x dx -> g x dx) -> OpenMap f -> OpenMap g
mapmapWithKey f = OpenMap . HashMap.mapWithKey go . unOpenMap
    where go key (SomeExpr y) = SomeExpr (f (OpenKey key) y)

lookup :: OpenMap f -> OpenKey x dx -> Maybe (f x dx)
lookup (OpenMap m) (OpenKey k) = unsafeCastType''' <$> HashMap.lookup k m


-- data SomeOpenItem f = forall x dx. SomeOpenItem (OpenKey x dx) (f x dx)
toList :: OpenMap f -> [SomeOpenItem f]
toList = fmap wrap . HashMap.toList . unOpenMap
    where wrap :: (StableName Any, SomeExpr f) -> SomeOpenItem f
          wrap (key, x) = case x of
              SomeExpr x' -> SomeOpenItem (OpenKey key) x'

unsafeCastType''' :: SomeExpr f -> f v dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x

intersectionWith :: forall f g h. (forall x dx. f x dx -> g x dx -> h x dx) -> OpenMap f -> OpenMap g -> OpenMap h
intersectionWith f (OpenMap x) (OpenMap y) = OpenMap (HashMap.intersectionWith f' x y)
    where f' (SomeExpr x') sy = SomeExpr (f x' y')
            where y' = unsafeCastType''' sy

insert :: forall f x dx. OpenKey x dx -> f x dx -> OpenMap f -> OpenMap f
insert (OpenKey k) x (OpenMap m) = OpenMap (HashMap.insert k (SomeExpr x) m)

empty :: OpenMap f
empty = OpenMap HashMap.empty
