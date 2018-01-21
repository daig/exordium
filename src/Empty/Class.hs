module Empty.Class (module Empty.Class, module X) where
import Map.Class as X
import Any as X

class Map f => Empty f where
  {-# minimal empty | void #-}
  empty :: f a
  empty = map (\case {}) void
  void :: f Any
  void = empty
