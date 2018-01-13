module Strong (Strong(..), ($:), module X) where
import Dimap as X
import Swap as X
import Map as X
import LinTraverse as X
import K
import I

class Dimap p => Strong p where
  {-# minimal lens | traversal_ | traversed_ | first | second #-}
  lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
  lens get set = \p -> dimap (\x -> (x,get x)) (\(s,b) -> set s b) (second p)
  {-lens get set = traversal_ (\afb s -> set s `map` afb (get s))-}
  traversal_ :: (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal_ f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})
  traversed_ :: LinTraverse t => p a b -> p (t a) (t b)
  traversed_ = traversal_ traverse_
  second :: p a b -> p (x,a) (x,b)
  {-second = \p -> dimap swap swap (first p)-}
  second = traversed_
  first :: p a b -> p (a,y) (b,y)
  {-first = lens (\(a,_) -> a) (\(_,c) b -> (b,c))-}
  {-first = traversal_ (\afb (a,y) -> (,y) `map` afb a)-}
  first = \p -> dimap swap swap (second p)

($:) :: Strong p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `postmap` first p

dimapDefault :: Strong p => (a -> x) -> (y -> b) -> p x y -> p a b
dimapDefault f g = traversal_ (\xfy a -> map g (xfy (f a)))

instance Strong (->) where
  lens get set f s = set s (f (get s))
  first f = \(a,y) -> (f a,y)
  second f = \(x,b) -> (x,f b)
