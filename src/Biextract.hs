module Biextract where
import Bimap
import Bool

class Bimap f => Biextract f where
  {-# minimal codiag | biextract #-}
  codiag :: f a a -> a
  codiag = biextract (\a -> a) (\a -> a)
  biextract :: (x -> a) -> (y -> a) -> f x y -> a
  biextract f g fab = codiag (bimap f g fab)
  
bimapCodiag :: (Biextract f, Eq a) => (x -> a) -> f x x -> Bool
bimapCodiag f faa = biextract f f faa == f (codiag faa)
