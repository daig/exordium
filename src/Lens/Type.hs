{-# language LiberalTypeSynonyms #-}
{-# language ImpredicativeTypes #-}
{-# language TypeInType #-}
module Lens.Type where
import I
import K
import Market as X (Market,Market')
import GHC.Types (type (*))

type Optic f s t a b = (a -> f b) -> s -> f t

-- Setter
type (s %~ a) b t = forall f. (IsI f) => (a -> f b) -> s -> f t
type s %~~ a = forall f. (IsI f) => (a -> f a) -> s -> f s
type (s %~. a) b t = (a -> I b) -> s -> I t
type s %~~. a = (a -> I a) -> s -> I s
-- TODO: something for multitarget setters?

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
-- Getter aka Linear Fold
type (s !?~ a)  = forall f. IsK f => (a -> f a) -> s -> f s
type (s ^~  a)  = forall f. IsK f => (a -> f a) -> s -> f s
type (s !?~. a) = (a -> K a a) -> s -> K a s
type (s ^~.  a) = (a -> K a a) -> s -> K a s

-- Prism
type (s +~ a) b t = forall p f. (Choice p, Pure f) => p a (f b) -> p s (f t)
type s +~~ a = forall p f. (Choice p, Pure f) => p a (f a) -> p s (f s)
type (s +~. a) b t = Market a b a (I b) -> Market a b s (I t)
type s +~~. a = Market' a a (I a) -> Market' a s (I s)

-- Review
type t |~ b = forall p f. (IsKK p, IsI f) => p b (f b) -> p t (f t)
type t |~. b = KK b (I b) -> KK t (I t)

-- Iso
type (s =~ a) b t = forall p f. (Dimap p, Map f) => p a (f b) -> p s (f t)
type (s =~~ a) = forall p f. (Dimap p, Map f) => p a (f a) -> p s (f s)
-- Improper Iso
type (s =#~ a) b t = forall p f. (Dimap p, Map f) => p a (f b) -> p s (f t)
type (s =#~~ a) = forall p f. (Dimap p, Map f) => p a (f a) -> p s (f s)

-- Equality
type ((s :: k1) ==~ (a :: k1)) (b :: k2) (t :: k2) = forall k3 (p :: k1 -> k3 -> *) (f :: k2 -> k3).
  p a (f b) -> p s (f t)
