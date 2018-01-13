module Traversed1 (Traversed1(..), module X) where
import Lens as X
import Apply as X
import Traverse1 as X
import Baz
import I

class Lens p => Traversed1 p where
  {-# minimal wander1 | traversed1 #-}
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander1 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: Traverse1 t => p a b -> p (t a) (t b)
  traversed1 = wander1 traverse1

instance Traversed1 (->) where wander1 l f s = case l (\a -> I (f a)) s of {I t -> t}

{-type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t-}
{-type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s-}
