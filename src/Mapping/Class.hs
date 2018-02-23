module Mapping.Class (Mapping(..), module X) where
import Traversal.Class as X
import Closed.Class as X
import Distributive.Class as X
import {-# source #-} I
import Mapping.Internal

class (Closed p, Traversal p) => Mapping p where
  {-# minimal mapping | mapped | distribution #-}
  mapping :: ((a -> b) -> s -> t) -> p a b -> p s t
  mapping f = \p -> dimap (Context (\x -> x)) (\(Context g s) -> f g s) (mapped p)
  -- I think this Distributive is equivalent to ((a -> b) -> s -> t). TODO: prove it
  distribution :: (forall f. (Distributive f) => (a -> f b) -> s -> f t) -> p a b -> p s t
  distribution afbsft = mapping (\ab s -> case afbsft (\a -> I (ab a)) s of I t -> t)
  {-distribution f = \p -> dimap (\s -> Bar (\afb -> f afb s))-}
                          {-(\(Bar k) -> (\(I x) -> x) (k I))-}
                          {-(mapped p)-}
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = distribution collect

instance Mapping (->) where mapping l = l

ff f = \p -> dimap (Context (\x -> x)) (\(Context g s) -> f g s) (distribution collect)
