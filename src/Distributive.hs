module Distributive where
import Map

class Map t => Distributive t where
  {-# minimal distribute | collect #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = distribute (map f a)

cotraverse :: (Distributive g, Map f) => (f a -> b) -> f (g a) -> g b
cotraverse f = \fga -> map f (distribute fga)

instance Distributive ((->) x) where
  collect axb fa = \x -> map (\a -> axb a x) fa
