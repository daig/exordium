module Fun where

(<) :: (x -> b) -> (a -> x) -> a -> b
(f < g) a = f (g a)
