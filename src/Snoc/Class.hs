module Snoc.Class (Snoc(..), module X) where
import Maybe
import Plus.F
import Traversed as X

class Snoc s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: Traversed' p => p (s,a) (t,b) -> p s t

instance Snoc [a] a b [b] where
   _Snoc = prism (\case {[] -> L []; as -> R (last (\l -> l) as)}) (\(bs,b) -> bs `fplus` [b])
last d [a] = (d [],a)
last d (a:as) = last (\l -> d (a:l)) as
