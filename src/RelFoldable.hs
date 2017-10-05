module RelFoldable (RelFoldable(..), module X) where
import Foldable as X
import Plus

class Foldable t => RelFoldable t where
  foldMap1 :: Plus s => (a -> s) -> t a -> s
