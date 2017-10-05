{-# OPTIONS_GHC -Wno-orphans #-}
module Prod where
import LinTraversable

instance LinTraversable ((,) x) where traverse_ f (x,a) = map (x,) (f a)
instance AffTraversable ((,) x) where traverse0 = traverse_
instance RelTraversable ((,) x) where traverse1 = traverse_
instance Traversable ((,) x) where traverse = traverse_
instance LinFoldable ((,) x) where foldMap_ = foldMap_Default
instance AffFoldable ((,) x) where foldMap0 = foldMap0Default
instance RelFoldable ((,) x) where foldMap1 = foldMap1Default
instance Foldable ((,) x) where foldMap = foldMapDefault
