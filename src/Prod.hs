{-# OPTIONS_GHC -Wno-orphans #-}
module Prod where
import LinTraverse

instance LinTraverse ((,) x) where traverse_ f (x,a) = (x,) `map` f a
instance AffTraverse ((,) x) where traverse0 = traverse_
instance RelTraverse ((,) x) where traverse1 = traverse_
instance Traverse ((,) x) where traverse = traverse_
instance LinFoldMap ((,) x) where foldMap_ = foldMap_Default
instance AffFoldMap ((,) x) where foldMap0 = foldMap0Default
instance RelFoldMap ((,) x) where foldMap1 = foldMap1Default
instance FoldMap ((,) x) where foldMap = foldMapDefault
