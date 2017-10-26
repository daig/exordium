module Ord 
  (module X
  ,(||),(&&)
  ,gt,ge,lt,le
  ) where
import Data.Ord as X (Ord(compare),Ordering(..))
import Data.Ord
import Data.Bool (Bool)

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
