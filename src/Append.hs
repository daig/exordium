module Append (Append(..), module X) where
import Map as X
import Sum as X (E)
import Sum

class Map f => Append f where
  (|+|) :: f a -> f b -> f (E a b)
  a |+| b = map L a `append` map R b
  appendWith :: (a -> c) -> (b -> c) -> f a -> f b -> f c
  appendWith f g a b = map (either f g) (a |+| b)
  append :: f a -> f a -> f a
  append = appendWith (\x -> x) (\x -> x)

instance Append [] where
  append [] bs = bs
  append (a:as) bs = a : append as bs
