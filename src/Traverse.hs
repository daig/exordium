module Traverse (Traverse(..),foldMapDefault,module X) where
import Map as X
import Apply as X
import FoldMap as X
import Pure as X
import K
import Plus
import Traversal as X

class (Map t,FoldMap t) => Traverse t where
  traverse :: (t a @~ a) b (t b)
  traverse f t = sequence (map f t)
  sequence :: (Pure f,Apply f) => t (f a) -> f (t a)
  sequence = traverse (\x -> x)

foldMapDefault :: (Traverse t, Zero m) => (a -> m) -> t a -> m
foldMapDefault f t = case traverse (\x -> K (f x)) t of {K m -> m}

instance Traverse ((,) x) where traverse f (x,a) = (x,) `map` f a
