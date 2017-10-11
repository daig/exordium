module LinTraverse (LinTraverse(..), foldMap_Default, module X) where
import Map as X
import LinFoldMap as X
import Apply as X
import AffTraverse as X
import RelTraverse as X
import K
import Plus

class (AffTraverse t, RelTraverse t,LinFoldMap t) => LinTraverse t where
  traverse_ :: Map f => (a -> f b) -> t a -> f (t b)
  traverse_ f t = sequence_ (map f t)
  sequence_ :: Map f => t (f a) -> f (t a)
  sequence_ = traverse_ (\x -> x)

foldMap_Default :: LinTraverse t => (a -> m) -> t a -> m
foldMap_Default f t = case traverse_ (\x -> K (f x)) t of {K m -> m}

