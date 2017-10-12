module Map where
import Trivial

class Map f where
  {-# minimal map #-}
  type MapC f :: * -> Constraint
  type MapC f = Trivial
  map :: (MapC f a,MapC f b) => (a -> b) -> f a -> f b
  constMap :: (MapC f a, MapC f b) => b -> f a -> f b
  constMap b = map (\_ -> b)

instance Map ((,) x) where map f (x,a) = (x,f a)
instance Map ((->) x) where map f g = \x -> f (g x)
