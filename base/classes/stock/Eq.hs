{-# language MagicHash #-}
module Eq (
  -- | Eq is the type of primitive structural equality
  -- The instance should exactly match that which would be derived.
  -- ie: Constructors should match exactly and recursively.
   eq#, ne#, Eq#
  ,Bool(..)
  ) where

import GHC.Types
import GHC.Classes

type Eq# = Eq
eq# :: Eq# a => a -> a -> Bool
{-# INLINE eq# #-}
infix 4 `eq#`
eq# = (==)
ne# :: Eq# a => a -> a -> Bool
{-# INLINE ne# #-}
ne# = (/=)
