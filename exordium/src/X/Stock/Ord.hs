{-# language MagicHash #-}
module X.Stock.Ord 
  (Ord#,compare#
  ,gt#,ge#,lt#,le#,min#,max#
  ,Ordering(..)
  {-,(||),(&&)-}
  ,module X
  ) where
import GHC.Classes
import GHC.Types
import X.Data.Bool as X

-- Ord is the type of primitive total orderings.
-- The instance should exactly match that which would be derived.
-- ie: Constructor order and left-to-right nesting priority.

{-(||) :: Ord a => a -> a -> a-}
{-(||) = max-}
{-(&&) :: Ord a => a -> a -> a-}
{-(&&) = min-}
type Ord# = Ord
compare# :: Ord# a => a -> a -> Ordering
compare# = compare
gt#,ge#,lt#,le# :: Ord# a => a -> a -> Bool
gt# = (>)
ge# = (>=)
lt# = (<)
le# = (<=)
min#,max# :: Ord# a => a -> a -> a
min# = min
max# = max

{-infixr 2 ||-}
{-infixr 3 &&-}
infix 4 `gt#`, `ge#`, `lt#`, `le#`
infixr 3 `min#`
infixr 2 `max#`
