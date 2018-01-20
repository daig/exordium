module Class.FoldMap0 (FoldMap0(..),module X) where
import Class.FoldMap as X
import Utils.Tuple

class FoldMap t => FoldMap0 t where
  foldMap0 :: Zero m => (a -> m) -> t a -> m

instance FoldMap0 ((,) x) where foldMap0 = tuple'foldMap
