module RelTraversing (RelTraversing(..), module X) where
import Strong as X
import Apply as X
import RelTraversable as X

class Strong p => RelTraversing p where
  {-# minimal wander1 #-}
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversing1 :: RelTraversable t => p a b -> p (t a) (t b)
  traversing1 = wander1 traverse1
