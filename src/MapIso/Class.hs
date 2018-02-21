module MapIso.Class where
import {-# source #-} O
import K.Type
import List
import K
import E
import Tuple
import Maybe

class MapIso f where mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((->) x) where mapIso _ f p = \a -> f (p a)
instance MapIso (K a) where mapIso _ _ = k'absurd
instance MapIso [] where mapIso _ = list'map
instance MapIso (E a) where mapIso _ = e'map
instance MapIso ((,) x) where mapIso _ = tuple'map
instance MapIso Maybe where mapIso _ = maybe'map
