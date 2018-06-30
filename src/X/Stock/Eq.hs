{-# language MagicHash #-}
module X.Stock.Eq (
  -- | Eq is the type of primitive structural equality
  -- The instance should exactly match that which would be derived.
  -- ie: Constructors should match exactly and recursively.
   eq#, neq#, Eq#
  ,module X
  ) where

import X.Data.Bool as X
import X.Stock

type Eq# = Eq
eq# :: Eq# a => a -> a -> Bool
infix 4 `eq#`
eq# = (==)
neq# :: Eq# a => a -> a -> Bool
neq# = (/=)
