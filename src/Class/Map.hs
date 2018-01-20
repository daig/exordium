module Class.Map (module Class.Map, module X) where
import Class.MapIso as X
import Type.O
import Utils.K
import Utils.List
import Utils.Where
import Utils.E
import Utils.Tuple
import Utils.I
import Type.Option
import Utils.Bazaar
{-import Utils.Baz-}

class MapIso f => Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)

map_mapIso :: Map f => (b -> a) -> (a -> b) -> f a -> f b
map_mapIso _ = map

{-map# :: forall b a f. (Map f, a #= b) => (a -> b) -> f a -> f b-}
{-map# _ = coerce#-}

instance Map ((->) x) where map f p = \a -> f (p a)
instance Map (K a) where map _ = k'absurd
instance Map [] where map = list'map
instance Map (Where a) where map = where'map
instance (Map f,Map g) => Map (O f g) where map f (O fg) = O (map (map f) fg)
instance Map (E a) where map = e'map
instance Map ((,) x) where map = tuple'map
instance Map (?) where map f = \case {None -> None; Some a -> Some (f a)}
instance Map I where map = i'map
instance Map (Bazaar Map a b) where map = bazaar'map map
instance MapIso (Bazaar Map a b) where mapIso _ = bazaar'map map

{-instance Map (Baz c t b) where map = baz'map-}
