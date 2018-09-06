module X.Functor.Fold.First (FoldFirst(..),module X) where
import X.Functor.Fold as X
import X.Num.Add.First

newtype FoldFirst f a = FoldFirst {unFoldFirst :: f a}

instance Fold f => Len (FoldFirst f) where len = foldMap_len -- TODO: fix this to use only Len constraint
instance Fold f => Fold (FoldFirst f) where foldMap = foldMap0
instance Fold f => Fold0 (FoldFirst f) where
  foldMap0 f (FoldFirst fa) = getFirst (foldMap (\a -> First (f a)) fa)
instance Fold1 f => StaticLen (FoldFirst f) where staticLen = fromNatural 1
instance Fold1 f => Fold1 (FoldFirst f) where foldMap1 = foldMap_
instance Fold1 f => Fold_ (FoldFirst f) where
  foldMap_ f (FoldFirst fa) = getFirst (foldMap1 (\a -> First (f a)) fa)
