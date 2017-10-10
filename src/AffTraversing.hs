module AffTraversing (AffTraversing(..), module X) where
{-import Traversing as X-}
import Choice as X
import Strong as X
import Pure as X
import AffTraversable as X
import Baz

class (Choice p, Strong p) => AffTraversing p where
  {-{-# minimal traversing0 #-}-}
  {-wander :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t-}
  {-wander f pab = dimap (\s -> Baz (\afb -> f afb s)) sold (traversing0 pab)-}
  {-traversing0 :: Traversable t => p a b -> p (t a) (t b)-}
  {-traversing0 = wander traverse0-}

{-firstDefault :: Traversing p => p a b -> p (a,y) (b,y)-}
{-firstDefault p = dimap swap swap (traverseP p)-}
{-secondDefault :: Traversing p => p a b -> p (x,a) (x,b)-}
{-secondDefault p = traverseP p-}

{-dimapDefault :: Traversing p => (a -> x) -> (y -> b) -> p x y -> p a b-}
{-dimapDefault f g = wander (\xfy a -> map g (xfy (f a)))-}
