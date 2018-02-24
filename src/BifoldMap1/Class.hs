module BifoldMap1.Class (BifoldMap1(..), module X) where
import PlusZero.Class as X
import BifoldMap.Class as X
import List
import Tuple

class BifoldMap t => BifoldMap1 t where
  bifoldMap1 :: Plus m => (a -> m) -> (b -> m) -> t a b -> m
   -- TODO ^ : check the order

instance BifoldMap1 (,) where bifoldMap1 f g (a,b) = f a `plus` g b
