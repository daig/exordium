module Map.Class (module Map.Class, module X) where
import MapIso.Class as X
import {-# source #-} K
import List
import Tuple

class MapIso f => Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)


instance Map ((->) x) where map f p = \a -> f (p a)
instance Map [] where map = list'map
instance Map ((,) x) where map = tuple'map
instance Map (K a) where map _ (K a) = K a
