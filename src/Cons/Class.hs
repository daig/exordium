module Cons.Class (module Cons.Class, module X) where
import Prism.Class as X
import Maybe
import AFold
import Prisms
import FPlus.Class

class Cons s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Cons :: Prism p => p (a,s) (b,t) -> p s t

instance Cons [a] a b [b] where
  _Cons = prism (\case {[] -> L []; a:as -> R (a,as)}) (\(b,bs) -> b:bs)

pattern (:<) :: Cons s a a s => a -> s -> s
pattern a :< s <- (foldMapOf _Cons Just -> Just (a,s))
  where a :< s = review _Cons (a,s)

class Snoc s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: Prism p => p (s,a) (t,b) -> p s t

instance Snoc [a] a b [b] where
   _Snoc = prism (\case {[] -> L []; as -> R (last (\l -> l) as)}) (\(bs,b) -> bs `fplus` [b])
last d [a] = (d [],a)
last d (a:as) = last (\l -> d (a:l)) as

pattern (:>) :: Snoc s a a s => s -> a -> s
pattern s :> a <- (foldMapOf _Snoc Just -> Just (s,a))
  where s :> a = review _Snoc (s,a)
