module X.Arrow.Mapped (Mapped(..), module X) where
import X.Arrow.Mapped.Internal
import X.Arrow.Traversed as X
import X.Arrow.Closed as X
import X.Type.I

class (Closed p, Traversed p) => Mapped p where
  {-# minimal mapping | mapped | setter #-}
  setter :: ((a -> b) -> s -> t) -> p a b -> p s t
  setter f = \p -> promap (Context (\x -> x)) (\(Context g s) -> f g s) (mapped p)
  -- I think this Zip is equivalent to ((a -> b) -> s -> t). TODO: prove it
  mapping :: (forall f. Zip f => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping afbsft = setter (\ab s -> case afbsft (\a -> I (ab a)) s of I t -> t)
  {-distribution f = \p -> promap (\s -> Bar (\afb -> f afb s))-}
                          {-(\(Bar k) -> (\(I x) -> x) (k I))-}
                          {-(mapped p)-}
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect

instance Mapped (->) where setter l = l
