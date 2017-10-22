module Grate where
import Iso as X
import Closed as X

type (s &~  a) b t = forall p. Closed p => p a b -> p s t
type  s &~~ a      = forall p. Closed p => p a a -> p s s
