module Ord 
  (module X
  ,(||),(&&)
  ) where
import Data.Ord as X (Ord(compare),Ordering(..))
import Data.Ord

(||) :: Ord a => a -> a -> a
(||) = max
(&&) :: Ord a => a -> a -> a
(&&) = min
