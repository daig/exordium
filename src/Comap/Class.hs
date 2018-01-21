{-# language MagicHash #-}
module Comap.Class where
import Type.K
import Coerce
import Map.Class

class MapIso f => Comap f where comap :: (b -> a) -> f a -> f b

comap_mapIso :: Comap f => (b -> a) -> (a -> b) -> f a -> f b
comap_mapIso f _ = comap f
