module Utils.Fun where

fun'dimap f g h = \a -> g (h (f a))
fun'lens get set f s = set s (f (get s))
fun'first f = \(a,y) -> (f a,y)
fun'second f = \(x,b) -> (x,f b)
