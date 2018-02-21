module Bimap.Class (module Bimap.Class, module X) where
import LMap.Class as X
import RMap.Class as X
import Tuple
import E

-- | Independently Map each on both sides
class (LMap p, RMap p) => Bimap p where
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = lmap f (rmap g p)
  {-bimap f g p = case map f (Flip (map g p)) of Flip fab -> fab -}


instance Bimap (,) where bimap = tuple'bimap
instance Bimap E where bimap = e'bimap
