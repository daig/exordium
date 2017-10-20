module RelTraversal
  (type (@!~), type (@!~~)
  ,module X) where
import Lens as X
import Apply as X

type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t
type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s
