module AffTraversal
  (type (@?~), type (@?~~)
  ,module X) where
import Lens as X
import Pure as X

type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t
type s @?~~ a       = forall f. Pure f => (a -> f a) -> s -> f s
