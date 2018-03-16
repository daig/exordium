module Num.Add.First (First(..), _First2, module X) where
import Num.Add0 as X
import Map.Pro as X

newtype First a = First {getFirst :: a}
  deriving anyclass Add0
  deriving newtype Zero
instance Add (First a) where First a `add` _ = First a

-- TODO: make a nice iso combinator for this
_First2 :: (First a -> First a -> First a) -> a -> a -> a
_First2 = promap First (promap First getFirst)
