{-# language MagicHash #-}
module Map (module Map, module X) where
import Map.Class as X
import Coerce as X (type (#=))
import Coerce (coerce#)
import Instances

defaulting 'mapIso [|\_ -> map|]

map# :: forall b a f. (Map f, a #= b) => f a -> f b
map# = coerce#
