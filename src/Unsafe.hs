{-# language MagicHash #-}
module Unsafe (Unsafe(..),module X) where
import Coerce as X (coerce#)
import Map as X

-- | A wrapper providing instances which ignore the normal datastructure contract
newtype Unsafe f a = Unsafe {getUnsafe# :: f a}

map# :: forall f a b. Map (Unsafe f) => (a -> b) -> f a -> f b
map# = mapAs# @(Unsafe f)
