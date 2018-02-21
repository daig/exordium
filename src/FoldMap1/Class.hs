module FoldMap1.Class (FoldMap1(..), module X) where
import FoldMap.Class as X
import Plus.Class
import Tuple

class FoldMap t => FoldMap1 t where
  foldMap1 :: Plus s => (a -> s) -> t a -> s

instance FoldMap1 ((,) x) where foldMap1 = tuple'foldMap
