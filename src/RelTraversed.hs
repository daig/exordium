module RelTraversed (RelTraversed(..), module X) where
import Strong as X
import Apply as X
import RelTraverse as X
import LinTraversed as X
import Baz

class LinTraversed p => RelTraversed p where
  {-# minimal wander1 | traversed1 #-}
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander1 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: RelTraverse t => p a b -> p (t a) (t b)
  traversed1 = wander1 traverse1
