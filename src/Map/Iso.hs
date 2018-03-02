module Map.Iso where
import List
import {-# source #-} K

class MapIso f where
  mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((->) x) where mapIso _ f p = \a -> f (p a)
instance MapIso [] where mapIso _ = list'map
instance MapIso ((,) x) where mapIso _ = (\f (x,y) -> (x,f y))
instance MapIso (K a) where mapIso _ _ (K a) = K a
