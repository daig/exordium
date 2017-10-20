module ALens
  (type (*~.), type (*~~.)
  ,module X) where
import ALens.Pretext as X (Pretext)

type (s *~. a) b t = (a -> Pretext (->) a b b) -> (s -> Pretext (->) a b t)
type s *~~. a = (a -> Pretext (->) a a a) -> (s -> Pretext (->) a a s)
