module MapL where

class MapL p where mapl :: (x -> a) -> p x b -> p a b

instance MapL (,) where mapl f (x,b) = (f x, b)
