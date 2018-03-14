{-# language MagicHash #-}
module Num.Zero (Zero(..), module X) where
import Num.Add as X
import GHC.Integer
import Bool
import Int
import Word

-- | zero + a = a + zero = a
class Add a => Zero a where
  zero :: a
  scale0 :: Natural -> a -> a
  scale0 0 = \_ -> zero
  scale0 n = scale1# n

instance Zero Natural where zero = 0
instance Zero Int where zero = 0
instance Zero Integer where zero = 0
instance Zero Word   where zero = 0
instance Zero Bool where zero = F
instance Zero () where zero = ()
