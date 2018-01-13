module RelTraversed (RelTraversed(..), module X) where
import Strong as X
import Apply as X
import RelTraverse as X
import Baz
import I

class Strong p => RelTraversed p where
  {-# minimal wander1 | traversed1 #-}
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander1 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: RelTraverse t => p a b -> p (t a) (t b)
  traversed1 = wander1 traverse1

instance RelTraversed (->) where wander1 l f s = case l (\a -> I (f a)) s of {I t -> t}
