module X.Functor.FPlus (FPlus(..),module X) where
import X.Functor.Remap as X
import X.Data.E as X
import X.Functor.Map

class Remap f => FPlus f where fplus :: f a -> f b -> f (E a b)
  {-choose :: (c -> E a b) -> (a -> c) -> (b -> c) -> f a -> f b -> f c-}
  {-choose f l r fa fb = remap f (\case {L a -> l a; R b -> r b}) (fplus fa fb)-}

instance FPlus [] where fplus a b = list'append (map L a) (map R b)

list'prepend,list'append :: [a] -> [a] -> [a]
list'prepend bs = go where
  go = \case
    [] -> bs
    a:as -> a : go as
list'append as bs = list'prepend bs as
