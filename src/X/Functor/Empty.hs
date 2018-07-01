{-# language MagicHash #-}
module X.Functor.Empty (module X.Functor.Empty, module X) where
{-import X.Num.Zero as X-}
{-import X.Functor.Map as X-}
import X.Data.Maybe
import X.Type.Any as X
import X.Cast.Coerce.Unsafe
import X.Data.X


-- TODO: Refactor this module
{-type Empty f = (FZero f, Absurd f)-}
class Empty f where empty :: f a
{-empty :: Empty f => f a-}
-- {-# INLINE empty #-}
{-empty = absurd fzero-}
  {-empty = map (\case {}) void-}
  {-void :: f Any-}
  {-void = empty-}

class Absurd f where
  absurd :: f X -> f a
  absurd = coerce#

instance Empty [] where empty = []
instance Empty Maybe where empty = Nothing

empty_zero :: Empty f => f a
empty_zero = empty
