module Lens (type (*~), type (*~~), module X) where
import Iso as X
import Strong as X

type (s *~  a) b t = forall p. Strong p => p a b -> p s t
type  s *~~ a = forall p. Strong p => p a a -> p s s
