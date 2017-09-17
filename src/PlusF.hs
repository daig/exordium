module PlusF where
import Map
import Bool


class PlusF f where (|+|) :: f a -> f a -> f a
assoc :: (Eq (f a), PlusF f) => f a -> f a -> f a -> Bool
assoc a b c = (a|+|b)|+|c == a|+|(b|+|c)

class PlusF f => Empty f where empty :: f a
mapEmpty :: forall f a b. (Eq (f b), Map f, Empty f) => (a -> b) -> Bool
mapEmpty f = map @f f empty == empty
plusEmpty :: (Eq (f a), PlusF f, Empty f) => f a -> Bool
plusEmpty a = (a |+| empty) == a && (empty |+| a) == a
