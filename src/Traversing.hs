module Traversing (Traversing(..)) where
import Dimap
import Choice
import Strong
import Applicative
import Traversable
import I


newtype Baz t b a = Baz {runBaz :: forall f. Applicative f => (a -> f b) -> f t}
instance Map (Baz t b) where map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))
instance Traversable (Baz t b) where
instance Foldable (Baz t b) where foldMap = foldMapDefault
sold :: Baz t a a -> t
sold m = case runBaz m I of {I t -> t}

class (Choice p, Strong p) => Traversing p where
  {-# minimal wander | traverseP #-}
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  wander f pab = dimap (\s -> Baz (\afb -> f afb s)) sold (traverseP pab)
  traverseP :: Traversable t => p a b -> p (t a) (t b)
  traverseP = wander traverse

firstDefault :: Traversing p => p a b -> p (a,y) (b,y)
firstDefault p = dimap swap swap (traverseP p)
secondDefault :: Traversing p => p a b -> p (x,a) (x,b)
secondDefault p = traverseP p

dimapDefault :: Traversing p => (a -> x) -> (y -> b) -> p x y -> p a b
dimapDefault f g = wander (\xfy a -> map g (xfy (f a)))
