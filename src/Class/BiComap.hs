module Class.BiComap (BiComap(..), module X) where
import Class.CoLMap as X
import Class.CoRMap as X

class (CoLMap p, CoRMap p) => BiComap p where
  bicomap :: (a -> x) -> (b -> y) -> p x y -> p a b
  bicomap f g p = cormap g (colmap f p)
