{-# language MagicHash #-}
module Num.Add0 (Add0(..), module X) where
import Num.Add as X
import Num.Zero as X
import GHC.Integer
import Bool
import Int
import Word

-- | zero + a = a + zero = a
class (Add a,Zero a) => Add0 a where
  scale0 :: Natural -> a -> a
  scale0 0 = \_ -> zero
  scale0 n = scale1# n

instance Add0 Natural
instance Add0 Integer
instance Add0 Int
instance Add0 Word
instance Add0 ()

instance (Add0 a, Add0 b) => Add0 (a,b)
