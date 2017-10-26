module LinTraversed
  (LinTraversed(..)
  ,firstDefault, secondDefault, lensDefault
  ,module X) where
import Strong as X
import Map as X
import LinTraverse as X
import Baz
import I

-- TODO: merge with Strong/Representable??
class Strong p => LinTraversed p where
  {-# minimal wander_ | traversed_ #-}
  wander_ :: (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander_ f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Map) (traversed_ pab)
  traversed_ :: LinTraverse t => p a b -> p (t a) (t b)
  traversed_ = wander_ traverse_

instance LinTraversed (->) where wander_ l f s = case l (\a -> I (f a)) s of {I t -> t}

firstDefault :: LinTraversed p => p a b -> p (a,y) (b,y)
firstDefault = \p -> dimap swap swap (traversed_ p)

secondDefault :: LinTraversed p => p a b -> p (x,a) (x,b)
secondDefault = traversed_

dimapDefault :: LinTraversed p => (a -> x) -> (y -> b) -> p x y -> p a b
dimapDefault f g = wander_ (\xfy a -> map g (xfy (f a)))

lensDefault :: LinTraversed p => (s -> a) -> (s -> b -> t) -> p a b -> p s t
lensDefault get set = wander_ (\afb s -> set s `map` afb (get s))
