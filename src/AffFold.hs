module AffFold
  (type (?~)
  ,module X) where
import Getter as X
import Pure as X

type s ?~ a = forall f. (Pure f, Comap f) => (a -> f a) -> s -> f s
