module FoldMap1 (FoldMap1(..), module X) where
import FoldMap as X
import Plus

class FoldMap t => FoldMap1 t where
  foldMap1 :: Plus s => (a -> s) -> t a -> s

instance FoldMap1 ((,) x) where foldMap1 f (x,a) = f a
