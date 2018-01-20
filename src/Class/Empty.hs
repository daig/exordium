module Class.Empty (module Class.Empty, module X) where
import Class.Map as X
import Type.X as X

class Map f => Empty f where
  {-# minimal empty | void #-}
  empty :: f a
  empty = map (\case {}) void
  void :: f X
  void = empty
