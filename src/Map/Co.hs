module Map.Co (Comap(..)) where
import {-# source #-} K

class Comap f where comap :: (b -> a) -> f a -> f b

instance Comap (K a) where comap _ (K a) = K a
