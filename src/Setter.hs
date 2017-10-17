module Setter
  (type (%~), type (%~~), type (%~.), type (%~~.)
  ,set, over, setter, cloneSetter
  ,module X) where
import IsI as X
import Coerce

-- | (%~) = forall f. IsI f => Optic f
type (s %~ a) b t = forall f. IsI f => (a -> f b) -> s -> f t
-- | (%~~) = forall f. IsI f => Optic' f
type s %~~ a = forall f. IsI f => (a -> f a) -> s -> f s
-- | (%~.) = Optic I
type (s %~. a) b t = (a -> I b) -> s -> I t
-- | (%~.) = Optic' I
type s %~~. a = (a -> I a) -> s -> I s

set :: (s %~. a) b t -> b -> s -> t
set l b = (\(I a) -> a) `postmap#` l (\_ -> I b)

over :: (s %~. a) b t -> (a -> b) -> s -> t
over l f = (\(I a) -> a) `postmap#` l (\a -> I (f a))

setter :: ((a -> b) -> s -> t) -> (s %~ a) b t
setter f g = taintedDot (f (untaintedDot g))

cloneSetter :: (s %~. a) b t -> (s %~ a) b t
cloneSetter l afb = taintedDot (postmap# (\(I a) -> a) (l (postmap# I (untaintedDot afb))))
