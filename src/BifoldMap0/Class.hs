module BifoldMap0.Class (BifoldMap0(..), module X) where
import BifoldMap.Class as X
import PlusZero.Class as X
import List
import Tuple

class BifoldMap t => BifoldMap0 t where
  bifoldMap0 :: Zero m => (a -> m) -> (b -> m) -> t a b -> m
