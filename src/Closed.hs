module Closed (Closed(..), ($.), zipped, module X) where
import Dimap as X
import Distributive as X

class Dimap p => Closed p where
  {-# minimal closed | grate #-}
  zipped :: Distributive f => p a b -> p (f a) (f b)
  zipped = collection collect -- grate (\f -> zipFWith f (\x -> x))
  closed :: p a b -> p (x -> a) (x -> b)
  closed = zipped -- grate (\g x -> g (\f -> f x))
  grate :: (((s -> a) -> b) -> t) -> p a b -> p s t
  grate f = \p -> dimap (\a g -> g a) f (closed p)
  collection :: (Distributive f => (a -> f b) -> s -> f t) -> p a b -> p s t
  -- collection = ???

  {-traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t-}


closed' :: Closed p => p a b -> p (x -> a) (x -> b)
closed' = zipped

instance Closed (->) where
  closed f = \xa x -> f (xa x)
  grate k f s = k (\sa -> f (sa s))

-- | curry
($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `premap` closed p

