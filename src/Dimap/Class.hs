module Dimap.Class (Dimap(..), module X) where
import Comap.L as X
import Map.R as X

class (ComapL p, MapR p) => Dimap p where
  dimap :: (a -> x) -> (y -> b) -> p x y -> p a b
  dimap f g = \p -> rmap g (colmap f p)

instance Dimap (->) where dimap f g p = \a -> g (p (f a))
