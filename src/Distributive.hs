module Distributive where
import Map

class Map t => Distributive t where
  {-# minimal distribute | collect #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = distribute (map f a)
