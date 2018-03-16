module Num.Ord 
  (Ord(compare)
  ,Ordering(..)
  ,(||),(&&)
  ,gt,ge,lt,le
  ,module X
  ) where
import Data.Ord
import Bool as X

-- Ord is the type of primitive total orderings
-- The instance should exactly match that which would be derived.
-- ie: Constructor order and left-to-right nesting priority.

(||) :: Ord a => a -> a -> a
(||) = max
(&&) :: Ord a => a -> a -> a
(&&) = min
gt,ge,lt,le :: Ord a => a -> a -> Bool
gt = (>)
ge = (>=)
lt = (<)
le = (<=)

infixr 2 ||
infixr 3 &&
infix 4 `gt`, `ge`, `lt`, `le`
