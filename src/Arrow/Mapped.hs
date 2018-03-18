module Arrow.Mapped (Mapped(..), module X) where
import Arrow.Mapped.Internal
import Arrow.Traversed as X
import Arrow.Closed as X
import Functor.Distribute as X
import Type.I

class (Closed p, Traversed p) => Mapped p where
  {-# minimal mapping | mapped | setter #-}
  setter :: ((a -> b) -> s -> t) -> p a b -> p s t
  setter f = \p -> promap (Context (\x -> x)) (\(Context g s) -> f g s) (mapped p)
  -- I think this Distribute is equivalent to ((a -> b) -> s -> t). TODO: prove it
  mapping :: (forall f. Distribute f => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping afbsft = setter (\ab s -> case afbsft (\a -> I (ab a)) s of I t -> t)
  {-distribution f = \p -> promap (\s -> Bar (\afb -> f afb s))-}
                          {-(\(Bar k) -> (\(I x) -> x) (k I))-}
                          {-(mapped p)-}
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect

instance Mapped (->) where setter l = l
