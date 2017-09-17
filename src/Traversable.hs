module Traversable where
import Map
import Apply
import Foldable
import Pure

class (Map t,Foldable t) => Traversable t where
  traverse :: (Pure f, Apply f) => (a -> f b) -> t a -> f (t b)
  traverse f t = sequence (map f t)
  sequence :: (Pure f,Apply f) => t (f a) -> f (t a)
  sequence = traverse (\x -> x)

{-class (Map t, Traversable t) => Traversable0 t wheer-}
  {-traverse0 :: Pure f => (a -> f b) -> t a -> f (t b)-}
  {-sequence0 :: -}
