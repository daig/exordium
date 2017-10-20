module RelFoldMap (RelFoldMap(..), module X) where
import FoldMap as X
import Plus

class FoldMap t => RelFoldMap t where
  foldMap1 :: Plus s => (a -> s) -> t a -> s

instance RelFoldMap ((,) x) where foldMap1 f (x,a) = f a
