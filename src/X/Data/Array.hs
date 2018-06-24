{-# language MagicHash #-}
module X.Data.Array (Array(..), type Array#, type MutableArray#) where

-- | Boxed arrays of boxed types
data Array a = Array# (Array# a)

-- | Boxed mutable arrays of boxed types, tracking the phantom thread state token
data MutableArray s a = MutableArray# (MutableArray# s a)
