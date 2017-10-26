module Negate (Negate(..), defaultMinus, module X) where
import Zero as X

class Zero a => Negate a where negate :: a -> a

defaultMinus :: Negate a => a -> a -> a
defaultMinus a b = a + negate b
