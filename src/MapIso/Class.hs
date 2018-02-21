module MapIso.Class where
import List
import E
import Tuple
import Maybe

class MapIso f where mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((->) x) where mapIso _ f p = \a -> f (p a)
instance MapIso [] where mapIso _ = list'map
instance MapIso (E a) where mapIso _ = e'map
instance MapIso ((,) x) where mapIso _ = tuple'map
instance MapIso Maybe where mapIso _ = maybe'map
