{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module OpenMap
    ( OpenKey, OpenMap
    , SomeOpenItem(SomeOpenItem)
    , mapmap, mapmapWithKey, lookup, toList
    , empty, insert, adjust
    , intersectionWith
    , makeOpenKey
)
where

import Prelude (Monad(return), IO, (.), Functor(fmap), (<$>), Maybe)
import GHC.StableName (StableName)
import Data.HashMap.Lazy (HashMap)
import GHC.Base (Any)
import qualified Data.HashMap.Lazy as HashMap
import Unsafe.Coerce (unsafeCoerce)
import Control.Exception (evaluate)
import System.Mem.StableName (makeStableName)
import Types (SomeExpr(SomeExpr))

newtype OpenKey dx = OpenKey (StableName Any)
newtype OpenMap f = OpenMap { unOpenMap :: HashMap (StableName Any) (SomeExpr f) }

data SomeOpenItem f = forall dx. SomeOpenItem (OpenKey dx) (f dx)

empty :: OpenMap f
empty = OpenMap HashMap.empty

mapmap :: forall f g. (forall dv. f dv -> g dv) -> OpenMap f -> OpenMap g
mapmap f = OpenMap . fmap go . unOpenMap
    where go (SomeExpr y) = SomeExpr (f y)

mapmapWithKey :: forall f g. (forall dx. OpenKey dx -> f dx -> g dx) -> OpenMap f -> OpenMap g
mapmapWithKey f = OpenMap . HashMap.mapWithKey go . unOpenMap
    where go key (SomeExpr y) = SomeExpr (f (OpenKey key) y)

lookup :: OpenMap f -> OpenKey dx -> Maybe (f dx)
lookup (OpenMap m) (OpenKey k) = unsafeCastType''' <$> HashMap.lookup k m


-- data SomeOpenItem f = forall x dx. SomeOpenItem (OpenKey x dx) (f x dx)
toList :: OpenMap f -> [SomeOpenItem f]
toList = fmap wrap . HashMap.toList . unOpenMap
    where wrap :: (StableName Any, SomeExpr f) -> SomeOpenItem f
          wrap (key, x) = case x of
              SomeExpr x' -> SomeOpenItem (OpenKey key) x'

unsafeCastType''' :: SomeExpr f -> f dv
unsafeCastType''' = \case
    SomeExpr x -> unsafeCoerce x

intersectionWith :: forall f g h. (forall dx. f dx -> g dx -> h dx) -> OpenMap f -> OpenMap g -> OpenMap h
intersectionWith f (OpenMap x) (OpenMap y) = OpenMap (HashMap.intersectionWith f' x y)
    where f' (SomeExpr x') sy = SomeExpr (f x' y')
            where y' = unsafeCastType''' sy

insert :: forall f dx. OpenKey dx -> f dx -> OpenMap f -> OpenMap f
insert (OpenKey k) x (OpenMap m) = OpenMap (HashMap.insert k (SomeExpr x) m)

adjust :: forall f dx. (f dx -> f dx) -> OpenKey dx -> OpenMap f -> OpenMap f
adjust f (OpenKey key) (OpenMap m) = OpenMap m'
    where m' = HashMap.adjust f' key m
          f' x = SomeExpr (f (unsafeCastType''' x))

makeOpenKey :: f dv -> IO (OpenKey dv)
makeOpenKey x = do
    x' <- evaluate x
    z <- makeStableName x'
    return (unsafeCoerce z)
