module Map.Co.R (ComapR(..),module X) where
import Map.Iso as X

class ComapR f where cormap :: (a -> b) -> f x b -> f x a
