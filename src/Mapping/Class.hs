module Mapping.Class (Mapping(..), module X) where
import Traversal.Class as X
import Closed.Class as X
import Distributive.Class as X
import I.Type
import Internal.Mapping

class (Closed p, Traversal p) => Mapping p where
  {-# minimal mapping | mapped #-}
  mapping :: (forall f. (Applicative f, Distributive f) => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping f = \p -> dimap (\s -> Bar (\afb -> f afb s))
                          (\(Bar k) -> (\(I x) -> x) (k I))
                          (mapped p)
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect
