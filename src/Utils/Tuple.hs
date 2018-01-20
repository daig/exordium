module Utils.Tuple where

tuple'bimap f g (x,y) = (f x, g y)
tuple'lmap = (`tuple'bimap` (\b -> b))
tuple'map = tuple'bimap (\a -> a)
tuple'swap (a,b) = (b,a)
tuple'foldMap f (_,x) = f x
tuple'traverse_ map = \f (x,a) -> (x,) `map` f a
