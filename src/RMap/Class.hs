module RMap.Class (module RMap.Class, module X) where
import Map.Class as X
import Tuple
import Star.Type
import K
import E
import These
import Where
import Flip

class RMap p where
  rmap :: (x -> b) -> p a x -> p a b
  {-default rmap :: forall a x b. Map (p a) => (x -> b) -> p a x -> p a b-}
  {-rmap = map-}


instance RMap (->) where rmap g p = \a -> g (p a)
instance RMap (,) where rmap = tuple'map
instance RMap K where rmap = k'map
instance RMap These where rmap = these'map
instance RMap Where where rmap = where'map
instance RMap E where rmap = e'map
instance RMap (Flipped K) where rmap g (Flip (K b)) = Flip (K (g b))
instance Map f => RMap (Star f) where rmap yb (Star afy) = Star (\a -> yb `map` afy a)
