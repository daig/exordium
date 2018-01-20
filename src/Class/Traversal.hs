module Class.Traversal (Traversal(..), module X) where
import Class.Traversal0 as X
import Class.Traversal1 as X
import Class.Applicative as X
import Class.Traverse as X
import Internal.Traversal
import Type.I

class (Traversal0 p, Traversal1 p) => Traversal p where
  {-# minimal traversal | traversed #-}
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse

instance Traversal (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}

