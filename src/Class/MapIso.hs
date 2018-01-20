module Class.MapIso where
import Type.O
import Type.K
import Type.These
import Utils.Where
import Utils.List
import Utils.K
import Utils.E
import Utils.Tuple
import Utils.Option
import Utils.I
{-import Utils.Baz-}

class MapIso f where mapIso :: (b -> a) -> (a -> b) -> f a -> f b

instance MapIso ((->) x) where mapIso _ f p = \a -> f (p a)
instance MapIso (K a) where mapIso _ _ = k'absurd
instance MapIso [] where mapIso _ = list'map
instance MapIso (Where a) where mapIso _ = where'map
instance (MapIso f,MapIso g) => MapIso (O f g) where
  mapIso f g (O fg) = O (mapIso (mapIso g f) (mapIso f g) fg)
instance MapIso (E a) where mapIso _ = e'map
instance MapIso ((,) x) where mapIso _ = tuple'map
instance MapIso (?) where mapIso _ = option'map
instance MapIso I where mapIso _ = i'map


{-instance MapIso (Baz c t b) where mapIso _ = baz'map-}
