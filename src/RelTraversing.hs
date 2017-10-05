module Traversing (Traversing(..)) where
import Dimap
import Choice
import Strong
import Applicative
import RelTraversable
import I


newtype Baz t b a = Baz {runBaz :: forall f. Applicative f => (a -> f b) -> f t}
instance Map (Baz t b) where map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))
instance RelTraversable (Baz t b) where
instance RelFoldable (Baz t b)
instance Traversable (Baz t b) where 
instance Foldable (Baz t b) where foldMap = foldMapDefault
sold :: Baz t a a -> t
sold m = case runBaz m I of {I t -> t}

class Strong p => RelTraversing p where
  {-# minimal wander1 | traverseP1 #-}
  wander1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander1 f pab = dimap (\s -> Baz (\afb -> f afb s)) sold (traverseP1 pab)
  traverseP1 :: RelTraversable t => p a b -> p (t a) (t b)
  traverseP1 = wander1 traverse1

firstDefault :: RelTraversing p => p a b -> p (a,y) (b,y)
firstDefault p = dimap swap swap (traverseP1 p)
secondDefault :: RelTraversing p => p a b -> p (x,a) (x,b)
secondDefault p = traverseP1 p
