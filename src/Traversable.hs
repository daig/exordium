module Traversable (Traversable(..),foldMapDefault,module X) where
import Map as X
import Apply as X
import Foldable as X
import Pure as X
import K
import Plus

class (Map t,Foldable t) => Traversable t where
  traverse :: (Pure f, Apply f) => (a -> f b) -> t a -> f (t b)
  traverse f t = sequence (map f t)
  sequence :: (Pure f,Apply f) => t (f a) -> f (t a)
  sequence = traverse (\x -> x)

foldMapDefault :: (Traversable t, Zero m) => (a -> m) -> t a -> m
foldMapDefault f t = case traverse (\x -> K (f x)) t of {K m -> m}

