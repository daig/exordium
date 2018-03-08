module Mapped.Class (Mapped(..), module X) where
import Traversed as X
import Closed.Class as X
import Distribute.Class as X
import I
import Mapped.Internal
import Star.Type

class (Closed p, Traversed p) => Mapped p where
  {-# minimal mapping | mapped | setter #-}
  setter :: ((a -> b) -> s -> t) -> p a b -> p s t
  setter f = \p -> dimap (Context (\x -> x)) (\(Context g s) -> f g s) (mapped p)
  -- I think this Distribute is equivalent to ((a -> b) -> s -> t). TODO: prove it
  mapping :: (forall f. (Applicative f,Distribute f) => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping afbsft = setter (\ab s -> case afbsft (\a -> I (ab a)) s of I t -> t)
  {-distribution f = \p -> dimap (\s -> Bar (\afb -> f afb s))-}
                          {-(\(Bar k) -> (\(I x) -> x) (k I))-}
                          {-(mapped p)-}
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect

instance Mapped (->) where setter l = l
instance (Distribute f) => Mapped (Star f) where
   mapped (Star f) = Star (collect f)
