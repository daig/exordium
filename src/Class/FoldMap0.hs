module Class.FoldMap0 (FoldMap0(..),module X) where
import Class.FoldMap as X
import Utils.Tuple
import Utils.I

class FoldMap t => FoldMap0 t where
  foldMap0 :: Zero m => (a -> m) -> t a -> m

instance FoldMap0 ((,) x) where foldMap0 = tuple'foldMap
instance FoldMap0 I where foldMap0 = i'foldMap_
