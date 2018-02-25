module Traversal1.Class (module Traversal1.Class, module X) where
import Traversed_.Class as X
import Apply.Class as X
import Traverse1.Class as X
import Traversal.Internal
import {-# source #-} I
import Star.Type

{-ff :: (s -> FunList a b t) -> (forall f. Apply f => (a -> f b) -> s -> f t)-}
{-ff sabt afb s = case sabt s of-}
  {-Done t -> gt-}
class Traversed_ p => Traversal1 p where
  {-# minimal traversal1 | traversed1 #-}
  {-funList :: (s -> FunList a b t) -> p a b -> p s t-}
  traversal1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal1 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: Traverse1 t => p a b -> p (t a) (t b)
  traversed1 = traversal1 traverse1

instance Traversal1 (->) where traversal1 l f s = case l (\a -> I (f a)) s of {I t -> t}
instance Apply f => Traversal1 (Star f) where traversal1 afbsft (Star afb) = Star (\s -> afbsft afb s)

{-type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t-}
{-type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s-}
