module AffFold
  (type (?~)
  ,module X) where
import Getter as X
import Traversed0 as X

type s ?~ a = forall p. (Traversed0 p, BiComap p) => p a a -> p s a
