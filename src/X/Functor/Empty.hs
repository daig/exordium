module X.Functor.Empty (module X.Functor.Empty, module X) where
{-import X.Num.Zero as X-}
{-import X.Functor.Map as X-}
import X.ADT.Maybe
import X.Type.Any as X


class Empty f where empty :: f a
  {-empty = map (\case {}) void-}
  {-void :: f Any-}
  {-void = empty-}

instance Empty [] where empty = []
instance Empty Maybe where empty = Nothing

empty_zero :: Empty f => f a
empty_zero = empty
