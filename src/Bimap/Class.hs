module Bimap.Class (module Bimap.Class, module X) where
import Map.L as X
import Map.R as X
import Tuple

-- | Independently Map each on both sides
class (MapL p, MapR p) => Bimap p where
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = lmap f (rmap g p)
  {-bimap f g p = case map f (Flip (map g p)) of Flip fab -> fab -}


instance Bimap (,) where bimap = tuple'bimap
