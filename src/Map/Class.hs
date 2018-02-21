module Map.Class (module Map.Class, module X) where
import MapIso.Class as X
import {-# source #-} O
import List
import E
import Tuple
import {-# source #-} Maybe

class MapIso f => Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)


instance Map ((->) x) where map f p = \a -> f (p a)
instance Map [] where map = list'map
instance Map (E a) where map = e'map
instance Map ((,) x) where map = tuple'map
instance Map Maybe where map f = \case {Nothing -> Nothing; Just a -> Just (f a)}
