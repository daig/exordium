module AffFold
  (type (?~)
  ,module X) where
import Getter as X
import AffTraversed as X

type s ?~ a = forall p. (AffTraversed p, BiComap p) => p a a -> p s a
