module Grate where
import Closed as X
import Distributive as X (Distributive)
import Distributive

type (s &~  a) b t = forall p. Closed p => p a b -> p s t
type  s &~~ a      = forall p. Closed p => p a a -> p s s

cotraversed :: Distributive f => (f a &~ a) b (f b)
cotraversed = grate (\f -> cotraverse f (\x -> x))

