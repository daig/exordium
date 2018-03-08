module Map.Co.R (ComapR(..),module X) where
import Map.Iso as X
import {-# source #-} K

class ComapR f where cormap :: (a -> b) -> f x b -> f x a

instance ComapR K where cormap _ (K a) = K a
