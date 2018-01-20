{-# language MagicHash #-}
module Utils.Map (module Utils.Map, module X) where
import Class.Map as X
import Coerce as X (type (#=))
import Coerce (coerce#)

map_mapIso :: Map f => (b -> a) -> (a -> b) -> f a -> f b
map_mapIso _ = map

map# :: forall b a f. (Map f, a #= b) => (a -> b) -> f a -> f b
map# _ = coerce#
