module FoldMap0.Class (FoldMap0(..),module X) where
import FoldMap.Class as X
import Tuple

class FoldMap t => FoldMap0 t where
  foldMap0 :: Zero m => (a -> m) -> t a -> m

instance FoldMap0 ((,) x) where foldMap0 = tuple'foldMap
