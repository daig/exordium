module Ord 
  (module X
  ,(||),(&&)
  ,gt,ge,lt,le
  ) where
import Data.Ord as X (Ord(compare),Ordering(..))
import Data.Ord
import Bool

(||) :: Ord a => a -> a -> a
(||) = max
(&&) :: Ord a => a -> a -> a
(&&) = min
gt,ge,lt,le :: Ord a => a -> a -> Bool
gt = (>)
ge = (>=)
lt = (<)
le = (<=)
