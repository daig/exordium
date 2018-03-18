module Functor.Fold.First (FoldFirst(..),module X) where
import Functor.Fold as X
import Coerce
import Num.Add.First

newtype FoldFirst f a = FoldFirst {unFoldFirst :: f a}

instance Fold f => Fold (FoldFirst f) where foldMap = foldMap0
instance Fold f => Fold0 (FoldFirst f) where
  foldMap0 f (FoldFirst fa) = getFirst (foldMap (\a -> First (f a)) fa)
instance Fold1 f => Fold1 (FoldFirst f) where foldMap1 = foldMap_
instance Fold1 f => Fold_ (FoldFirst f) where
  foldMap_ f (FoldFirst fa) = getFirst (foldMap1 (\a -> First (f a)) fa)
