module Mapping (Mapping(..), module X) where
import Traversing as X
import Closed as X
import Distributive as X
import I
import Mapping.Bar

class (Closed p, Traversing p) => Mapping p where
  {-# minimal mapping | roam #-}
  roam :: (forall f. (Applicative f, Distributive f) => (a -> f b) -> s -> f t) -> p a b -> p s t
  roam f = \p -> dimap (\s -> Bar (\afb -> f afb s)) (\(Bar k) -> (\(I x) -> x) (k I)) (mapping p)
  mapping :: Map f => p a b -> p (f a) (f b)
  mapping = roam collect
