module Setter
  (type (%~), type (%~~)
  ,setter --, cloneSetter
  ,module X) where
import IsI as X
import Coerce

-- | (%~) = forall f. IsI f => Optic f
type (s %~ a) b t = forall f. IsI f => (a -> f b) -> s -> f t
-- | (%~~) = forall f. IsI f => Optic' f
type s %~~ a = forall f. IsI f => (a -> f a) -> s -> f s

setter :: ((a -> b) -> s -> t) -> (s %~ a) b t
setter f g = taintedDot (f (untaintedDot g))

{-cloneSetter :: (s %~. a) b t -> (s %~ a) b t-}
{-cloneSetter l afb = taintedDot (postmap# (\(I a) -> a) (l (postmap# I (untaintedDot afb))))-}
