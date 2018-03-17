{-# language MagicHash #-}
module Num.One where
import GHC.Natural
import GHC.Integer
import Type.Word
import Bool
import Type.Int

-- | one * a = a * one = a
class One m where one :: m

instance One Natural where one = 1
instance One Integer where one = 1
instance One Word where one = 1
instance One Int where one = 1
instance One () where one = ()
instance One Bool where one = T
