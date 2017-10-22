module Closed (Closed(..), ($.), cotraversed, module X) where
import Dimap as X
import Distributive as X

class Dimap p => Closed p where
  {-# minimal closed | grate #-}
  closed :: p a b -> p (x -> a) (x -> b)
  closed = grate (\g x -> g (\f -> f x))
  grate :: (((s -> a) -> b) -> t) -> p a b -> p s t
  grate f = \p -> dimap (\a g -> g a) f (closed p)

cotraversed :: (Distributive f, Closed p) => p a b -> p (f a) (f b)
cotraversed = grate (\f -> cotraverse f (\x -> x))

instance Closed (->) where
  closed f = \xa x -> f (xa x)
  grate k f s = k (\sa -> f (sa s))

-- | curry
($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `premap` closed p

