module Cons (module Cons, module X) where
import Cons.Class as X
import Num.Zero as X
import Optic.Prism
import Maybe
import Traversed

pattern (:<) :: Cons s a a s => a -> s -> s
pattern a :< s <- (_View _Cons Just -> Just (a,s))
  where a :< s = _Review _Cons (a,s)

view_ :: (Cons x a a x, Zero x0)
        => (View (x -> x) a a -> View (x0 -> t) s s) -> s -> t
{-toListOf :: (s ^~.. a) ([a] -> [a]) -> s -> [a]-}
view_ l s = _View l (:<) s zero
