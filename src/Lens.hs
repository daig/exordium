module Lens (type (*~), type (*~~), lens, module X) where
import Iso as X
import Map as X

type (s *~   a) b t = forall f. Map f => (a -> f b) -> s -> f t
type s *~~   a      = forall f. Map f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> b -> t) -> (s *~ a) b t
lens sa sbt afb s = sbt s `map` afb (sa s)
