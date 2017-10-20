module Bimap.Laws where
import Bimap
import Bool
import Ord

bimapId :: (Bimap p, Eq (p a b)) => p a b -> Bool
bimapId p = bimap (\x -> x) (\y -> y) p == p
lmaprBimap :: (Eq (p a b), Bimap p) => (x -> a) -> (y -> b) -> p x y -> Bool
lmaprBimap f g p = lr == rl && lr == bimap f g p
  where
    lr = mapl f (mapr g p)
    rl = mapr g (mapl f p)
distrib :: (Bimap p, Eq (p a b)) => (a' -> a) -> (x -> a') -> (b' -> b) -> (y -> b') -> p x y -> Bool
distrib f g h i a =
  bimap (\x -> f (g x)) (\y -> h (i y)) a
  == bimap f h (bimap g i a)
