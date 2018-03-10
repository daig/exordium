module Map.Iso where
import List
import {-# source #-} K
import {-# source #-} I
import {-# source #-} E

class MapIso f where
  mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((->) x) where mapIso _ f p = \a -> f (p a)
instance MapIso [] where mapIso _ = list'map
instance MapIso ((,) x) where mapIso _ = (\f (x,y) -> (x,f y))
instance MapIso (K a) where mapIso _ _ (K a) = K a
instance MapIso I where mapIso _ f (I a) = I (f a)
instance MapIso (E x) where
  mapIso _ f = \case
    L a -> L a
    R b -> R (f b)
