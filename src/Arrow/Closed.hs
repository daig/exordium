module Arrow.Closed (module Arrow.Closed, module X) where
import Arrow.Promap as X
import Distribute as X

class Promap p => Closed p where
  {-# minimal zipped | closed | grate | collection #-}
  zipped :: Distribute f => p a b -> p (f a) (f b)
  zipped = collection zipF -- grate (\f -> zipF f (\x -> x))
  closed :: p a b -> p (x -> a) (x -> b)
  closed = zipped -- grate (\g x -> g (\f -> f x))
  grate :: (((s -> a) -> b) -> t) -> p a b -> p s t
  grate f = \p -> promap (\a g -> g a) f (closed p)
  collection :: (forall f. Map f => (f a -> b) -> f s -> t) -> p a b -> p s t
  collection sabsst = grate (`sabsst` (\x -> x))

  {-traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t-}

instance Closed (->) where
  closed f = \xa x -> f (xa x)
  grate k f s = k (\sa -> f (sa s))


-- | curry
($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `premap` closed p
