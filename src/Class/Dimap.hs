module Class.Dimap (module Class.Dimap, module X) where
import Class.CoLMap as X
import Class.RMap as X
import Type.K
import Flip

class (CoLMap p, RMap p) => Dimap p where
  dimap :: (a -> x) -> (y -> b) -> p x y -> p a b
  dimap f g = \p -> rmap g (colmap f p)

instance Dimap (->) where dimap f g p = \a -> g (p (f a))
instance Dimap (Flipped K) where dimap _ g (Flip (K b)) = Flip (K (g b))
