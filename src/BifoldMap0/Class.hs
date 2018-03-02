module BifoldMap0.Class (BifoldMap0(..), module X) where
import BifoldMap.Class as X

class BifoldMap t => BifoldMap0 t where
  bifoldMap0 :: Zero m => (a -> m) -> (b -> m) -> t a b -> m
