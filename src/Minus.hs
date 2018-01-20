{-# language PostfixOperators #-}
module Minus (Minus(..), module X) where
import Class.PlusZero as X

-- a - a = zero
-- (a - b) - c = a - (b + c)
class PlusZero a => Minus a where
  {-# minimal minus | negate #-}
  minus :: a -> a -> a
  a `minus` b = a `plus` negate b
  negate :: a -> a
  negate = minus zero
