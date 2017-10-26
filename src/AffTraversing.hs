module AffTraversed (AffTraversed(..), module X) where
{-import Traversed as X-}
import Choice as X
import Strong as X
import Pure as X
import AffTraverse as X
import Baz

class (Choice p, Strong p) => AffTraversed p where
  {-# minimal traversing0 #-}
  wander0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  {-wander0 f pab = dimap (\s -> Baz (\afb -> f afb s)) sold (traversing0 pab)-}
  traversing0 :: AffTraverse t => p a b -> p (t a) (t b)
  traversing0 = wander0 traverse0

{-firstDefault :: Traversed p => p a b -> p (a,y) (b,y)-}
{-firstDefault p = dimap swap swap (traverseP p)-}
{-secondDefault :: Traversed p => p a b -> p (x,a) (x,b)-}
{-secondDefault p = traverseP p-}

{-dimapDefault :: Traversed p => (a -> x) -> (y -> b) -> p x y -> p a b-}
{-dimapDefault f g = wander (\xfy a -> map g (xfy (f a)))-}

{-firstDefault :: RelTraversed p => p a b -> p (a,y) (b,y)-}
{-firstDefault = \p -> dimap swap swap (traversing1 p)-}

{-secondDefault :: RelTraversed p => p a b -> p (x,a) (x,b)-}
{-secondDefault = traversing1-}

{-lensDefault :: RelTraversed p => (s -> a) -> (s -> b -> t) -> p a b -> p s t-}
{-lensDefault get set = wander1 (\afb s -> set s `map` afb (get s))-}
