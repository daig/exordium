module Traversing (Traversing(..), module X) where
import Choice as X
import Strong as X
import Applicative as X
import Traverse as X
import Baz
import I

class (Choice p, Strong p) => Traversing p where
  {-# minimal wander | traversing #-}
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander f pab = dimap (\s -> Baz (\afb -> f afb s)) sold (traversing pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = wander traverse

instance Traversing (->) where wander l f s = case l (\a -> I (f a)) s of {I t -> t}
{-firstDefault :: Traversing p => p a b -> p (a,y) (b,y)-}
{-firstDefault p = dimap swap swap (traverseP p)-}
{-secondDefault :: Traversing p => p a b -> p (x,a) (x,b)-}
{-secondDefault p = traverseP p-}

{-dimapDefault :: Traversing p => (a -> x) -> (y -> b) -> p x y -> p a b-}
{-dimapDefault f g = wander (\xfy a -> map g (xfy (f a)))-}
