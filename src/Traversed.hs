module Traversed (Traversed(..), module X) where
import Traversed0 as X
import Traversed1 as X
import Applicative as X
import Traverse as X
import Baz
import I

class (Traversed0 p, Traversed1 p) => Traversed p where
  {-# minimal traversal | traversed #-}
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse

instance Traversed (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}

traversed_dimap :: Traversed p => (a -> x) -> (y -> b) -> p x y -> p a b
traversed_dimap f g = traversal (\xfy a -> map g (xfy (f a)))
