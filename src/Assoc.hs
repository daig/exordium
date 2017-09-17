module Assoc where
import Bimap
import Bool

class Bimap f => Assoc f where
  assoc :: f a (f b c) -> f (f a b) c
  assoc' :: f (f a b) c -> f a (f b c)

assoc'assoc :: (Assoc f, Eq (f a (f b c))) => f a (f b c) -> Bool
assoc'assoc fafbc = assoc' (assoc fafbc) == fafbc
commutes :: (Assoc f, Eq (f (f a b) c)) => (x -> a) -> (y -> b) -> (z -> c) -> f x (f y z) -> Bool
commutes f g h a = assoc (bimap f (bimap g h) a) == bimap (bimap f g) h (assoc a)

instance Assoc (,) where
  assoc (a,(b,c)) = ((a,b),c)
  assoc' ((a,b),c) = (a,(b,c))
