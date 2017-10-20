module MapL where

class MapL p where mapL :: (x -> a) -> p x b -> p a b

instance MapL (,) where mapL f (x,b) = (f x, b)
