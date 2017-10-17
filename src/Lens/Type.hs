{-# language LiberalTypeSynonyms #-}
{-# language ImpredicativeTypes #-}
{-# language TypeInType #-}
module Lens.Type where
import I
import K
import Market as X (Market,Market')
import GHC.Types (type (*))



-- Traversal
type (s @~ a) b t   = forall f. Applicative f => (a -> f b) -> s -> f t
--Relevant Traversal
type (s @!~ a) b t  = forall f. Apply f => (a -> f b) -> s -> f t
--Affine Traversal
type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t
-- Lens aka Linear Traversal
type (s @!?~ a) b t = forall f. Map f => (a -> f b) -> s -> f t
type (s *~   a) b t = forall f. Map f => (a -> f b) -> s -> f t
type s @!?~~ a      = forall f. Map f => (a -> f a) -> s -> f s
type s *~~   a      = forall f. Map f => (a -> f a) -> s -> f s

-- Fold
type s .~ a = forall f. (IsK f, Applicative f) => (a -> f a) -> s -> f s
-- Getting
type (s .~. a) m = (a -> K m a) -> s -> K m s
--Relevant Fold
type s !~ a = forall f. (IsK f, Apply f) => (a -> f a) -> s -> f s
--Affine Fold
type (s ?~ a) b t = forall f. (IsK f, Pure f) => (a -> f b) -> s -> f t
-- Linear fold
type (s !?~ a)  = forall f. IsK f => (a -> f a) -> s -> f s
type (s !?~. a) = (a -> K a a) -> s -> K a s


-- Equality
type ((s :: k1) ==~ (a :: k1)) (b :: k2) (t :: k2) = forall k3 (p :: k1 -> k3 -> *) (f :: k2 -> k3).
  p a (f b) -> p s (f t)
