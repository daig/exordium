module Map.Iso where
import List
import Tuple
import {-# source #-} K
import Coerce

class MapIso f where
  mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((->) x) where mapIso _ f p = \a -> f (p a)
instance MapIso [] where mapIso _ = list'map
instance MapIso ((,) x) where mapIso _ = tuple'map
instance MapIso (K a) where mapIso _ _ (K a) = K a
