module Class.FoldMap1 (FoldMap1(..), module X) where
import Class.FoldMap as X
import Class.Plus
import Utils.Tuple

class FoldMap t => FoldMap1 t where
  foldMap1 :: Plus s => (a -> s) -> t a -> s

instance FoldMap1 ((,) x) where foldMap1 = tuple'foldMap
