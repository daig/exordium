module Mono.Cons (module Mono.Cons, module X) where
import Num.Zero as X
import Optic.Prism
import Maybe
import Arrow.Traversed
import Functor.Plus

class Cons s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Cons :: Traversed' p => p (a,s) (b,t) -> p s t

instance Cons [a] a b [b] where
  _Cons = prism (\case {[] -> L []; a:as -> R (a,as)}) (\(b,bs) -> b:bs)

pattern (:<) :: Cons s a a s => a -> s -> s
pattern a :< s <- (_View _Cons Just -> Just (a,s))
  where a :< s = _Review _Cons (a,s)

view_ :: (Cons x a a x, Zero x0)
        => (View (x -> x) a a -> View (x0 -> t) s s) -> s -> t
{-toListOf :: (s ^~.. a) ([a] -> [a]) -> s -> [a]-}
view_ l s = _View l (:<) s zero
