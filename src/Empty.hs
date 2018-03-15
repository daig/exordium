module Empty (module Empty, module X) where
{-import Num.Zero as X-}
{-import Map as X-}
import Maybe
import Any as X


class Empty f where empty :: f a
  {-empty = map (\case {}) void-}
  {-void :: f Any-}
  {-void = empty-}

instance Empty [] where empty = []
instance Empty Maybe where empty = Nothing

empty_zero :: Empty f => f a
empty_zero = empty
