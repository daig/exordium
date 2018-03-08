module Map.Co.Bi (BiComap(..),module X) where
import Map.Co.L as X
import Map.Co.R as X

class (ComapL p, ComapR p) => BiComap p where
  bicomap :: (a -> x) -> (b -> y) -> p x y -> p a b
  bicomap f g p = cormap g (colmap f p)
