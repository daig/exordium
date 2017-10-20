module RelTraversing (RelTraversing(..), module X) where
import Strong as X
import Apply as X
import RelTraverse as X

class Strong p => RelTraversing p where
  {-# minimal wander1 #-}
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversing1 :: RelTraverse t => p a b -> p (t a) (t b)
  traversing1 = wander1 traverse1

newtype RelBaz t b a = RelBaz {runRelBaz :: forall f. Apply f => (a -> f b) -> f t}

firstDefault :: RelTraversing p => p a b -> p (a,y) (b,y)
firstDefault = \p -> dimap swap swap (traversing1 p)

secondDefault :: RelTraversing p => p a b -> p (x,a) (x,b)
secondDefault = traversing1

lensDefault :: RelTraversing p => (s -> a) -> (s -> b -> t) -> p a b -> p s t
lensDefault get set = wander1 (\afb s -> set s `map` afb (get s))
