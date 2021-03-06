{-# language MagicHash #-}
module X.Mono.Snoc where
import X.Data.Maybe
import X.Optic.Review
import X.Optic.View
import X.Functor.Append

class Snoc s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: Traversed' p => p (s,a) (t,b) -> p s t

instance Snoc [a] a b [b] where
   _Snoc = prism (\case {[] -> L []; as -> R (last# (\l -> l) as)}) (\(bs,b) -> bs `append` [b])
     where
        last# d = \case
          [a] -> (d [],a)
          a:as -> last# (\l -> d (a:l)) as
          [] -> __

pattern (:>) :: Snoc s a a s => s -> a -> s
pattern s :> a <- (_View _Snoc Just -> Just (s,a))
  where s :> a = _Review _Snoc (s,a)
