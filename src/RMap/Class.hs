module RMap.Class (module RMap.Class, module X) where
import Map.Class as X
import Tuple
import Star.Type

class RMap p where
  rmap :: (x -> b) -> p a x -> p a b
  {-default rmap :: forall a x b. Map (p a) => (x -> b) -> p a x -> p a b-}
  {-rmap = map-}


instance RMap (->) where rmap g p = \a -> g (p a)
instance RMap (,) where rmap = tuple'map
instance Map f => RMap (Star f) where rmap yb (Star afy) = Star (\a -> yb `map` afy a)
