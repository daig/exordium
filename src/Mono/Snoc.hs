{-# language MagicHash #-}
module Mono.Snoc where
import Maybe
import Optic.Review
import Optic.View
import Plus.F

class Snoc s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: Traversed' p => p (s,a) (t,b) -> p s t

instance Snoc [a] a b [b] where
   _Snoc = prism (\case {[] -> L []; as -> R (last# (\l -> l) as)}) (\(bs,b) -> bs `fplus` [b])
     where
        last# d = \case
          [a] -> (d [],a)
          a:as -> last# (\l -> d (a:l)) as
          [] -> __

pattern (:>) :: Snoc s a a s => s -> a -> s
pattern s :> a <- (_View _Snoc Just -> Just (s,a))
  where s :> a = _Review _Snoc (s,a)
