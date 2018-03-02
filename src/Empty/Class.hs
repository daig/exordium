module Empty.Class (module Empty.Class, module X) where
import Map.Class as X
import Zero.Class as X
import Forall as X
import Maybe
import Any as X

class (Map f,ForallF Zero f) => Empty f where
  {-# minimal empty | void #-}
  empty :: f a
  empty = map (\case {}) void
  void :: f Any
  void = empty

instance Empty [] where empty = []
instance Empty Maybe where empty = Nothing
