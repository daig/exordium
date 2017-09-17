module PlusF where
import Map
import Bool
import Sum
import Biextract
import Assoc


class Map f => PlusF f where
  (|+|) :: f a -> f b -> f (E a b)
  appendWith :: (a -> c) -> (b -> c) -> f a -> f b -> f c
  appendWith f g a b = map (biextract f g) (a |+| b)
  append :: f a -> f a -> f a
  append a b = map codiag (a |+| b)

testAssoc :: (Map f, Eq (f (E (E a b) c)), PlusF f) => f a -> f b -> f c -> Bool
testAssoc a b c = (a|+|b)|+|c == map assoc (a|+|(b|+|c))

class PlusF f => Empty f where empty :: f a
mapEmpty :: forall f a b. (Eq (f b), Map f, Empty f) => (a -> b) -> Bool
mapEmpty f = map @f f empty == empty
plusEmpty :: forall f x a. (Eq (f (E x a)), Eq (f (E a x)), Empty f) => f a -> Bool
plusEmpty a = (a |+| empty @f @x) == map L a && (empty @f @x |+| a) == map R a
