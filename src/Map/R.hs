module Map.R (module Map.R, module X) where
import Map as X
import Tuple
import Star.Type

class MapR p where
  rmap :: (x -> b) -> p a x -> p a b
  {-default rmap :: forall a x b. Map (p a) => (x -> b) -> p a x -> p a b-}
  {-rmap = map-}


instance MapR (->) where rmap g p = \a -> g (p a)
instance MapR (,) where rmap = tuple'map
instance Map f => MapR (Star f) where rmap yb (Star afy) = Star (\a -> yb `map` afy a)

rmap_map :: MapR p => (x -> b) -> p a x -> p a b
rmap_map = rmap
