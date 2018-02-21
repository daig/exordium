module Map.Class (module Map.Class, module X) where
import MapIso.Class as X
import {-# source #-} O
import K
import List
import Where
import E
import Tuple
import Maybe.Type

class MapIso f => Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)


instance Map ((->) x) where map f p = \a -> f (p a)
instance Map (K a) where map _ = k'absurd
instance Map [] where map = list'map
instance Map (Where a) where map = where'map
instance (Map f,Map g) => Map (O f g) where map f (O fg) = O (map (map f) fg)
instance Map (E a) where map = e'map
instance Map ((,) x) where map = tuple'map
instance Map Maybe where map f = \case {Nothing -> Nothing; Just a -> Just (f a)}
