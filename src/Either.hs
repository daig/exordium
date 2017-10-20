module Either where
import Bimap

class Bimap f => Either f where
  {-# minimal codiag | either #-}
  codiag :: f a a -> a
  codiag = either (\a -> a) (\a -> a)
  either :: (x -> a) -> (y -> a) -> f x y -> a
  either f g fab = codiag (bimap f g fab)
