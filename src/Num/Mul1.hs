{-# language MagicHash #-}
module Num.Mul1 (Mul1(..), module X) where
import Num.Mul as X
import Num.One as X
import GHC.Integer
import Word
import Bool
import Int

-- | one * a = a * one = a
class (Mul m, One m) => Mul1 m where
  pow0 :: Natural -> m -> m
  pow0 0 = \_ -> one
  pow0 n = pow1# n

instance Mul1 Natural
instance Mul1 Integer
instance Mul1 Word
instance Mul1 Int
instance Mul1 ()
instance Mul1 Bool
