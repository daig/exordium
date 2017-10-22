module Bimap where

-- | Independently Map each on both sides
class Bimap p where
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = rmap g (lmap f p)
  lmap :: (x -> a) -> p x b -> p a b
  lmap f = bimap f (\b -> b)
  rmap :: (x -> b) -> p a x -> p a b
  rmap = bimap (\a -> a)

instance Bimap (,) where
  bimap f g (x,y) = (f x, g y)
