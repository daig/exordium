module Bitraverse (Bitraverse(..),bifoldMapDefault,module X) where
import Bimap as X
import Applicative as X
import BiFoldMap as X
import K
import Plus

class (Bimap t,BiFoldMap t) => Bitraverse t where
  bitraverse :: (Pure f, Apply f) => (x -> f a) -> (y -> f b) -> t x y -> f (t a b)
  bitraverse f g t = bisequence (bimap f g t)
  bisequence :: Applicative f => t (f a) (f b) -> f (t a b)
  bisequence = bitraverse (\x -> x) (\y -> y)

bifoldMapDefault :: (Bitraverse t, Zero m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault f g t = case bitraverse (\x -> K (f x)) (\y -> K (g y)) t of {K m -> m}
