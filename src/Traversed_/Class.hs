module Traversed_.Class (module Traversed_.Class, module X) where
import Dimap.Class as X
import Map.Class as X
import Traverse_.Class as X
import K
import I
import Star.Type
import Swap

class Dimap p => Traversed_ p where
  {-# minimal lens | traversal_ | traversed_ | first | second #-}
  lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
  lens get set = \p -> dimap (\x -> (x,get x)) (\(s,b) -> set s b) (second p)
  {-lens get set = traversal_ (\afb s -> set s `map` afb (get s))-}
  traversal_ :: (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal_ f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})
  traversed_ :: Traverse_ t => p a b -> p (t a) (t b)
  traversed_ = traversal_ traverse_
  second :: p a b -> p (x,a) (x,b)
  {-second = \p -> dimap swap swap (first p)-}
  second = traversed_
  first :: p a b -> p (a,y) (b,y)
  {-first = lens (\(a,_) -> a) (\(_,c) b -> (b,c))-}
  {-first = traversal_ (\afb (a,y) -> (,y) `map` afb a)-}
  first = \p -> dimap swap swap (second p)

traversal__dimap :: Traversed_ p => (a -> x) -> (y -> b) -> p x y -> p a b
traversal__dimap f g = traversal_ (\xfy a -> map g (xfy (f a)))

instance Traversed_ (->) where
  lens sa sbt ab = \s -> sbt s (ab (sa s))
  first f (a,x) = (f a,x)
  second f (x,a) = (x, f a)

instance Map f => Traversed_ (Star f) where traversal_ afbsft (Star afb) = Star (\s -> afbsft afb s)
{-instance Optic Traversed_ where data A Traversed_ a b s t = Traversed_ (s -> a) (s -> b -> t)-}
{-instance Traversed_ (A Traversed_ a b) where-}
  {-first (Traversed_ x y) = Traversed_ (\(a,_) -> x a) (\(s,c) b -> (y s b,c))-}
{-instance Dimap (A Traversed_ a b) where-}
  {-dimap f g (Traversed_ x y) = Traversed_ (\s -> x (f s)) (\s b -> g (y (f s) b))-}

{-data A Traversed_ p a b t = Pretext {runPretext :: forall f. Map f => p a (f b) -> f t}-}
