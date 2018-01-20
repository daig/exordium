{-# language PostfixOperators #-}
module Minus (Minus(..), module X) where
import Class.PlusZero as X
import Option as X (type (?))
import Option


-- a - a = zero
-- (a - b) - c = a - (b + c)
class PlusZero a => Minus a where
  {-# minimal (-) | negate #-}
  (-) :: a -> a -> a
  a - b = a + negate b
  negate :: a -> a
  negate a = zero - a
