module Class.Traversal1 (module Class.Traversal1, module X) where
import Class.Lens as X
import Class.Apply as X
import Class.Traverse1 as X
import Baz
import Type.I
import Type.O

{-ff :: (s -> FunList a b t) -> (forall f. Apply f => (a -> f b) -> s -> f t)-}
{-ff sabt afb s = case sabt s of-}
  {-Done t -> gt-}
class Lens p => Traversal1 p where
  {-# minimal traversal1 | traversed1 #-}
  {-funList :: (s -> FunList a b t) -> p a b -> p s t-}
  traversal1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal1 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: Traverse1 t => p a b -> p (t a) (t b)
  traversed1 = traversal1 traverse1

instance Traversal1 (->) where traversal1 l f s = case l (\a -> I (f a)) s of {I t -> t}

{-type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t-}
{-type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s-}
