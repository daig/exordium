module Mapped.Class (Mapped(..), module X) where
import Traversed.Class as X
import Closed.Class as X
import Distribute.Class as X
import {-# source #-} I
import Mapped.Internal
import Star.Type

class (Closed p, Traversed p) => Mapped p where
  {-# minimal mapping | mapped | distribution #-}
  mapping :: ((a -> b) -> s -> t) -> p a b -> p s t
  mapping f = \p -> dimap (Context (\x -> x)) (\(Context g s) -> f g s) (mapped p)
  -- I think this Distribute is equivalent to ((a -> b) -> s -> t). TODO: prove it
  distribution :: (forall f. (a -> f b) -> s -> f t) -> p a b -> p s t
  distribution afbsft = mapping (\ab s -> case afbsft (\a -> I (ab a)) s of I t -> t)
  {-distribution f = \p -> dimap (\s -> Bar (\afb -> f afb s))-}
                          {-(\(Bar k) -> (\(I x) -> x) (k I))-}
                          {-(mapped p)-}
  mapped :: Map f => p a b -> p (f a) (f b)
  {-mapped = distribution collect-}

instance Mapped (->) where mapping l = l
instance (Distribute f ) => Mapped (Star f) where
   mapped (Star f) = Star (collect f)
