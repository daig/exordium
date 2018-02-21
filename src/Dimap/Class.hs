module Dimap.Class (Dimap(..), module X) where
import CoLMap.Class as X
import RMap.Class as X
import Re.Type
import Star.Type

class (CoLMap p, RMap p) => Dimap p where
  dimap :: (a -> x) -> (y -> b) -> p x y -> p a b
  dimap f g = \p -> rmap g (colmap f p)

instance Dimap (->) where dimap f g p = \a -> g (p (f a))
instance Map f => Dimap (Star f) where dimap ax yb (Star xfy) = Star (\a -> yb `map` xfy (ax a))
instance Dimap p => Dimap (Re p s t) where
  dimap f g (Re l) = Re (\p -> l (dimap g f p))
instance CoLMap p => RMap (Re p s t) where rmap f (Re l) = Re (\p -> l (colmap f p))
instance RMap p => CoLMap (Re p s t) where colmap f (Re l) = Re (\p -> l (rmap f p))
