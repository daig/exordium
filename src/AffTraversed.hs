module AffTraversed (AffTraversed(..), module X) where
{-import Traversed as X-}
import Choice as X
import Strong as X
import Pure as X
import AffTraverse as X
import LinTraversed as X
import Baz

class (Choice p, LinTraversed p) => AffTraversed p where
  {-# minimal wander0 | traversed0 #-}
  wander0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander0 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)
  traversed0 :: AffTraverse t => p a b -> p (t a) (t b)
  traversed0 = wander0 traverse0

{-dimapDefault :: Traversed p => (a -> x) -> (y -> b) -> p x y -> p a b-}
{-dimapDefault f g = wander (\xfy a -> map g (xfy (f a)))-}
