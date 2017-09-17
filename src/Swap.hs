module Swap where

class Bimap p => Swap p where swap :: p a b -> p b a
swapSwap :: (Eq (p a b),Swap p) => p a b -> Bool
swapSwap a = swap (swap a) == a
swapBimap :: (Swap p, Eq (p b a)) => (x -> a) -> (y -> b) -> p x y -> Bool
swapBimap f g p = swap (bimap f g p) == bimap g f (swap p)

instance Swap (,) where swap (a,b) = (b,a)
