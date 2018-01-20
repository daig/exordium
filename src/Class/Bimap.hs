module Class.Bimap (module Class.Bimap, module X) where
import Class.LMap as X
import Class.RMap as X
import Utils.Tuple
import Utils.K
import Utils.These
import Utils.Where
import Utils.E

-- | Independently Map each on both sides
class (LMap p, RMap p) => Bimap p where
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = lmap f (rmap g p)
  {-bimap f g p = case map f (Flip (map g p)) of Flip fab -> fab -}


instance Bimap (,) where bimap = tuple'bimap
instance Bimap K where bimap = k'bimap
instance Bimap These where bimap = these'bimap
instance Bimap Where where bimap = where'bimap
instance Bimap E where bimap = e'bimap
