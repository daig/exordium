module Traversal.Class (Traversal(..), module X) where
import Traversal0.Class as X
import Traversal1.Class as X
import Applicative.Class as X
import Traverse.Class as X
import Traversal.Internal
import {-# source #-} I
import Star.Type

class (Traversal0 p, Traversal1 p) => Traversal p where
  {-# minimal traversal | traversed #-}
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse

instance Traversal (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}

instance Applicative f => Traversal (Star f) where traversal afbsft (Star afb) = Star (\s -> afbsft afb s)
