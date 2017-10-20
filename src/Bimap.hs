module Bimap
  (Bimap(..)
  ,module X) where
import Bool
import Ord
import MapL as X
import MapR as X

-- | Independently Map each on both sides
class (MapL p, MapR p) => Bimap p where
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = rmap g (lmap f p)
  lmap :: (x -> a) -> p x b -> p a b
  lmap f = bimap f (\b -> b)
  rmap :: (x -> b) -> p a x -> p a b
  rmap = bimap (\a -> a)
bimapId :: (Bimap p, Eq (p a b)) => p a b -> Bool
bimapId p = bimap (\x -> x) (\y -> y) p == p
lrmapBimap :: (Eq (p a b), Bimap p) => (x -> a) -> (y -> b) -> p x y -> Bool
lrmapBimap f g p = lr == rl && lr == bimap f g p
  where
    lr = mapl f (mapr g p)
    rl = mapr g (mapl f p)
distrib :: (Bimap p, Eq (p a b)) => (a' -> a) -> (x -> a') -> (b' -> b) -> (y -> b') -> p x y -> Bool
distrib f g h i a =
  bimap (\x -> f (g x)) (\y -> h (i y)) a
  == bimap f h (bimap g i a)


instance Bimap (,) where
  bimap f g (x,y) = (f x, g y)
