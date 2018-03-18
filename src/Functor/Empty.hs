module Functor.Empty (module Functor.Empty, module X) where
{-import Num.Zero as X-}
{-import Functor.Map as X-}
import ADT.Maybe
import Type.Any as X


class Empty f where empty :: f a
  {-empty = map (\case {}) void-}
  {-void :: f Any-}
  {-void = empty-}

instance Empty [] where empty = []
instance Empty Maybe where empty = Nothing

empty_zero :: Empty f => f a
empty_zero = empty
