module Lens
  (type (*~), type (*~~)
  ,lens'
  ,module X) where
import Iso as X
import Strong as X

type (s *~  a) b t = forall p. Strong p => p a b -> p s t
type  s *~~ a      = forall p. Strong p => p a a -> p s s

lens' :: (s -> (a,b -> t)) -> (s *~ a) b t
lens' f = lens (\(f -> (a,_)) -> a) (\(f -> (_,g)) -> g)
