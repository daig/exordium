module FoldMap0.Class (FoldMap0(..),module X) where
import FoldMap.Class as X

class FoldMap t => FoldMap0 t where
  foldMap0 :: Zero m => (a -> m) -> t a -> m

instance FoldMap0 ((,) x) where foldMap0 f (_,x) = f x
