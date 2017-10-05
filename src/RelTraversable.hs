module RelTraversable (RelTraversable(..), foldMap1Default, module X) where
import Map as X
import RelFoldable as X
import Apply as X
import Traversable as X
import K
import Plus

class (Traversable t,RelFoldable t) => RelTraversable t where
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  traverse1 f t = sequence1 (map f t)
  sequence1 :: Apply f => t (f a) -> f (t a)
  sequence1 = traverse1 (\x -> x)

foldMap1Default :: (RelTraversable t, Plus m) => (a -> m) -> t a -> m
foldMap1Default f t = case traverse1 (\x -> K (f x)) t of {K m -> m}
