module Cons.Class (Cons(..), module X) where
import Prism.Class as X
import Maybe
import AFold
import Plus.F

class Cons s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Cons :: Prism p => p (a,s) (b,t) -> p s t

instance Cons [a] a b [b] where
  _Cons = prism (\case {[] -> L []; a:as -> R (a,as)}) (\(b,bs) -> b:bs)
