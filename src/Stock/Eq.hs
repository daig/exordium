{-# language MagicHash #-}
module Stock.Eq (
  -- | Eq is the type of primitive structural equality
  -- The instance should exactly match that which would be derived.
  -- ie: Constructors should match exactly and recursively.
   eq#, neq#, Eq#
  ,module X
  ) where

import Bool as X
import Stock

type Eq# = Eq
eq# :: Eq# a => a -> a -> Bool
eq# = (==)
neq# :: Eq# a => a -> a -> Bool
neq# = (/=)

