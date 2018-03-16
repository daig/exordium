module FoldMap.First (FoldingFirst(..),module X) where
import FoldMap as X
import Coerce
import Num.Add.First

newtype FoldingFirst f a = FoldingFirst {unFoldingFirst :: f a}

instance FoldMap f => FoldMap (FoldingFirst f) where foldMap = foldMap0
instance FoldMap f => FoldMap0 (FoldingFirst f) where
  foldMap0 f (FoldingFirst fa) = getFirst (foldMap (\a -> First (f a)) fa)
instance FoldMap1 f => FoldMap1 (FoldingFirst f) where foldMap1 = foldMap_
instance FoldMap1 f => FoldMap_ (FoldingFirst f) where
  foldMap_ f (FoldingFirst fa) = getFirst (foldMap1 (\a -> First (f a)) fa)
