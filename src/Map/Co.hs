module Map.Co (Comap(..),module X) where
import Map.Iso as X
import {-# source #-} K

class MapIso f => Comap f where comap :: (b -> a) -> f a -> f b

comap_mapIso :: Comap f => (b -> a) -> (a -> b) -> f a -> f b
comap_mapIso f _ = comap f

instance Comap (K a) where comap _ (K a) = K a
