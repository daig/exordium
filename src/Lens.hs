module Lens where
import Map
import Comap
import Bimap
import Pure
import Dimap
import Apply

type Lens s t a b = forall f. Map f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. (Pure f, Apply f) => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

-- | An Affine Traversal has at most one target
type AffTraversal s t a b = forall f. (Pure f, Map f) => (a -> f b) -> s -> f t
type AffTraversal' s a = AffTraversal s s a a

-- | A Relevant Traversal has at least one target
type RelTraversal s t a b = forall f. Apply f => (a -> f b) -> s -> f t
type RelTraversal' s a = RelTraversal s s a a

-- | A Linear Traversal has exactly one target
type LinTraversal s t a b = forall f. Map f => (a -> f b) -> s -> f t
type LinTraversal' s a = LinTraversal s s a a

type Fold s t a b = forall f. (Comap f, Pure f, Apply f) => (a -> f b) -> s -> f t
type Fold' s a = Fold s s a a

type Prism s t a b = forall p f. (Choice p, Pure f, Apply f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

type Review t b = forall p f. (Choice p, Bimap p, Map f) => p b (f b) -> p t (f t)


{-coerced :: forall s t a b. (s =# a, t =# b) => Iso s t a b-}
{-coerced -}
