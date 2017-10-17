module Append (Append(..), module X) where
import Map as X
import Sum as X (E)
import Sum

class Map f => Append f where
  (|+|) :: f a -> f b -> f (E a b)
  appendWith :: (a -> c) -> (b -> c) -> f a -> f b -> f c
  appendWith f g a b = map (either f g) (a |+| b)
  append :: f a -> f a -> f a
  append a b = map codiag (a |+| b)

