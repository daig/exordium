{-# language MagicHash #-}
module Map (module Map, module X) where
import Map.Class as X
import Coerce as X (type (#=))
import Coerce (coerceF,coerceF#)

map_mapIso :: Map f => (b -> a) -> (a -> b) -> f a -> f b
map_mapIso _ = map

mapAs :: forall g f a b. (Map g, f a #= g a, g b #= f b) => (a -> b) -> f a -> f b
mapAs f fa = coerceF @f (map f (coerceF @g fa))

mapAs# :: forall g f a b. Map g => (a -> b) -> f a -> f b
mapAs# f fa = coerceF# @f (map f (coerceF# @g fa))
