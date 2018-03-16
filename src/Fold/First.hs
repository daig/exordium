module Fold.First (FoldingFirst(..),module X) where
import Fold as X
import Coerce
import Num.Add.First

newtype FoldingFirst f a = FoldingFirst {unFoldingFirst :: f a}

instance Fold f => Fold (FoldingFirst f) where foldMap = foldMap0
instance Fold f => Fold0 (FoldingFirst f) where
  foldMap0 f (FoldingFirst fa) = getFirst (foldMap (\a -> First (f a)) fa)
instance Fold1 f => Fold1 (FoldingFirst f) where foldMap1 = foldMap_
instance Fold1 f => Fold_ (FoldingFirst f) where
  foldMap_ f (FoldingFirst fa) = getFirst (foldMap1 (\a -> First (f a)) fa)
