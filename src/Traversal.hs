module Traversal
  (type (@~), type (@~~)
  ,module X) where
import AffTraversal as X
import RelTraversal as X
import Applicative as X

type (s @~ a) b t = forall f. Applicative f => (a -> f b) -> s -> f t
type s @~~ a      = forall f. Applicative f => (a -> f a) -> s -> f s
