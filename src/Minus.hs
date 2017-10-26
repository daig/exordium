{-# language PostfixOperators #-}
module Minus (Minus(..), defaultNegate, module X) where
import Def as X
import Option as X (type (?))
import Option


-- a - a = def
-- (a - b) - c = a - (b + c)
class Def a => Minus a where (-) :: a -> a -> a

defaultNegate :: Minus a => a -> a
defaultNegate = \a -> def - a
