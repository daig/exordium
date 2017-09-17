module Map where
import Bool

class Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)
mapId :: (Map f, Eq (f a)) => f a -> Bool
mapId a = map (\x -> x) a == a
distrib :: (Map f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
distrib f g a = map (\x -> g (f x)) a == map g (map f a)
