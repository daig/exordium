module Closed.Class (Closed(..), module X) where
import Map.Di as X
import Distribute.Class as X
import Star.Type

class Dimap p => Closed p where
  {-# minimal zipped | closed | grate | collection #-}
  zipped :: Distribute f => p a b -> p (f a) (f b)
  zipped = collection zipF -- grate (\f -> zipF f (\x -> x))
  closed :: p a b -> p (x -> a) (x -> b)
  closed = zipped -- grate (\g x -> g (\f -> f x))
  grate :: (((s -> a) -> b) -> t) -> p a b -> p s t
  grate f = \p -> dimap (\a g -> g a) f (closed p)
  collection :: (forall f. Map f => (f a -> b) -> f s -> t) -> p a b -> p s t
  collection sabsst = grate (`sabsst` (\x -> x))

  {-traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t-}

instance Closed (->) where
  closed f = \xa x -> f (xa x)
  grate k f s = k (\sa -> f (sa s))

instance Distribute f => Closed (Star f) where
  closed (Star afb) = Star (\xa -> distribute (\x -> afb (xa x)))



