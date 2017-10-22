module Traversal
  (type (@~), type (@~~)
  ,module X) where
import AffTraversal as X
import RelTraversal as X
import Applicative as X

type (s @~  a) b t = forall p. Traversing p => p a b -> p s t
type  s @~~ a      = forall p. Traversing p => p a a -> p s s
