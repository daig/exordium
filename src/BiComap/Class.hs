module BiComap.Class (BiComap(..), module X) where
import CoLMap.Class as X
import CoRMap.Class as X

class (CoLMap p, CoRMap p) => BiComap p where
  bicomap :: (a -> x) -> (b -> y) -> p x y -> p a b
  bicomap f g p = cormap g (colmap f p)
