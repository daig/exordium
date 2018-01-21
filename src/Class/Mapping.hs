module Class.Mapping (Mapping(..), module X) where
import Class.Traversal as X
import Class.Closed as X
import Class.Distributive as X
import Type.I
import Internal.Mapping

class (Closed p, Traversal p) => Mapping p where
  {-# minimal mapping | mapped #-}
  mapping :: (forall f. (Applicative f, Distributive f) => (a -> f b) -> s -> f t) -> p a b -> p s t
  mapping f = \p -> dimap (\s -> Bar (\afb -> f afb s))
                          (\(Bar k) -> (\(I x) -> x) (k I))
                          (mapped p)
  mapped :: Map f => p a b -> p (f a) (f b)
  mapped = mapping collect
