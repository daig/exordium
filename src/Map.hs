module Map where

class Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)
