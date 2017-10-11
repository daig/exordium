module Bitraverse (Bitraverse(..),bifoldMapDefault,module X) where
import Map as X
import Applicative as X
import FoldMap as X
import K
import Plus

class (Map t,FoldMap t) => Traverse t where
  bitraverse :: (Pure f, Apply f) => (a -> f b) -> t a -> f (t b)
  bitraverse f t = bisequence (bimap f g t)
  bisequence :: Applicative f => t (f a) (f b) -> f (t a b)
  bisequence = bitraverse (\x -> x) (\y -> y)

foldMapDefault :: (Traverse t, Zero m) => (a -> m) -> (b -> m) -> t a b -> m
foldMapDefault f g t = case bitraverse (\x -> K (f x)) (\y -> K (g y)) t of {K m -> m}
