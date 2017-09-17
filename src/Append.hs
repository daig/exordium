module Append where
import Map
import Ord
import Bool
import Sum
import Biextract
import Assoc
import Empty


class Map f => Append f where
  (|+|) :: f a -> f b -> f (E a b)
  appendWith :: (a -> c) -> (b -> c) -> f a -> f b -> f c
  appendWith f g a b = map (biextract f g) (a |+| b)
  append :: f a -> f a -> f a
  append a b = map codiag (a |+| b)

testAssoc :: (Map f, Eq (f (E (E a b) c)), Append f) => f a -> f b -> f c -> Bool
testAssoc a b c = (a|+|b)|+|c == map assoc (a|+|(b|+|c))

plusEmpty :: forall f x a. (Eq (f (E x a)), Eq (f (E a x)), Append f, Map f, Empty f) => f a -> Bool
plusEmpty a = (a |+| empty @f @x) == map L a && (empty @f @x |+| a) == map R a
