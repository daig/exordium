module Append.Laws where
import Append
import Ord
import Bool
import Sum
import Assoc
import Empty

testAssoc :: (Map f, Eq (f (E (E a b) c)), Append f) => f a -> f b -> f c -> Bool
testAssoc a b c = (a|+|b)|+|c == map assoc (a|+|(b|+|c))

plusEmpty :: forall f x a. (Eq (f (E x a)), Eq (f (E a x)), Append f, Map f, Empty f) => f a -> Bool
plusEmpty a = (a |+| empty @f @x) == map L a && (empty @f @x |+| a) == map R a
