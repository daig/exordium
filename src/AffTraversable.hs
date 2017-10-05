module AffTraversable where
import Map
import AffFoldable
import Pure
import Traversable
import K
import Def

class (Traversable t,AffFoldable t) => AffTraversable t where
  traverse0 :: Pure f => (a -> f b) -> t a -> f (t b)
  traverse0 f t = sequence0 (map f t)
  sequence0 :: Pure f => t (f a) -> f (t a)
  sequence0 = traverse0 (\x -> x)

foldMap0Default :: (AffTraversable t, Def m) => (a -> m) -> t a -> m
foldMap0Default f t = case traverse0 (\x -> K (f x)) t of {K m -> m}
