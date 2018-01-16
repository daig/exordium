module Mapping (Mapping(..), module X) where
import Traversal as X
import Closed as X
import Distributive as X
import I
import Mapping.Bar

class (Closed p, Traversal p) => Mapping p where
  {-# minimal mapping | mapped #-}
  mapping :: (forall f. (Applicative f, Distributive f) => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping f = \p -> dimap (\s -> Bar (\afb -> f afb s))
                          (\(Bar k) -> (\(I x) -> x) (k I))
                          (mapped p)
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect
