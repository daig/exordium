module Biextract where
import Bimap

class Bimap f => Biextract f where
  {-# minimal codiag | biextract #-}
  codiag :: f a a -> a
  codiag = biextract (\a -> a) (\a -> a)
  biextract :: (x -> a) -> (y -> a) -> f x y -> a
  biextract f g fab = codiag (bimap f g fab)
