module Cons (module Cons, module X) where
import Cons.Class as X
import Zero as X
import Forget as X
import Maybe
import AFold
import Prism
import Optic.TH

pattern (:<) :: Cons s a a s => a -> s -> s
pattern a :< s <- (foldMapOf _Cons Just -> Just (a,s))
  where a :< s = review _Cons (a,s)

view_ :: (Cons x a a x, Zero x0)
        => (Forget (x -> x) a a -> Forget (x0 -> t) s s) -> s -> t
{-toListOf :: (s ^~.. a) ([a] -> [a]) -> s -> [a]-}
view_ l s = viewWith (:<) l s zero
