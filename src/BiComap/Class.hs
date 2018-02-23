module BiComap.Class (BiComap(..), module X) where
import Comap.L as X
import Comap.R as X

class (ComapL p, ComapR p) => BiComap p where
  bicomap :: (a -> x) -> (b -> y) -> p x y -> p a b
  bicomap f g p = cormap g (colmap f p)
