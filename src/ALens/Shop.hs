module ALens.Shop where
import Lens as X

data Shop a b s t = Shop (s -> a) (s -> b -> t)
instance Lens (Shop a b) where
  first (Shop x y) = Shop (\(a,_) -> x a) (\(s,c) b -> (y s b,c))
instance Dimap (Shop a b) where
  dimap f g (Shop x y) = Shop (\s -> x (f s)) (\s b -> g (y (f s) b))
