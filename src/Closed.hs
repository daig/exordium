module Closed (Closed(..), ($.), module X) where
import Dimap as X

class Dimap p => Closed p where
  {-# minimal closed | grate #-}
  closed :: p a b -> p (x -> a) (x -> b)
  closed = grate (\g x -> g (\f -> f x))
  grate :: (((s -> a) -> b) -> t) -> p a b -> p s t
  grate f = \p -> dimap (\a g -> g a) f (closed p)

-- | curry
($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `premap` closed p
