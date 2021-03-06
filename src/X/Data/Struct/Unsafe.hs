{-# language MagicHash #-}
module X.Data.Struct.Unsafe where

-- | A wrapper providing instances which ignore the normal datastructure contract
newtype Unsafe f a = Unsafe {getUnsafe# :: f a}

-- TODO: prevent collision with Map.map#
{-map# :: forall f a b. Map (Unsafe f) => (a -> b) -> f a -> f b-}
{-map# = mapAs# @(Unsafe f)-}
