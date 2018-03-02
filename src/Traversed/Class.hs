module Traversed.Class (Traversed(..), module X) where
import Traversed0.Class as X
import Traversed1.Class as X
import Applicative.Class as X
import Traverse.Class as X
import Traversed.Internal
import I
import Star.Type

class (Traversed0 p, Traversed1 p) => Traversed p where
  {-# minimal traversal | traversed #-}
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse

instance Traversed (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}

instance Applicative f => Traversed (Star f) where traversal afbsft (Star afb) = Star (\s -> afbsft afb s)
