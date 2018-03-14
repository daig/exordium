{-# language MagicHash #-}
module Num.One (One(..), module X) where
import Num.Mul as X
import GHC.Integer
import Word
import Bool
import Int

-- | one * a = a * one = a
class Mul m => One m where
  one :: m
  pow0 :: Natural -> m -> m
  pow0 0 = \_ -> one
  pow0 n = pow1# n

instance One Natural where one = 1
instance One Integer where one = 1
instance One Word where one = 1
instance One Int where one = 1
instance One () where one = ()
instance One Bool where one = T
