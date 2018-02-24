module BifoldMap_.Class (BifoldMap_(..), module X) where
import BifoldMap0.Class as X
import BifoldMap1.Class as X

class (BifoldMap0 t, BifoldMap1 t) => BifoldMap_ t where bifoldMap_ :: (x -> a) -> (y -> a) -> t x y -> a
