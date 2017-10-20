module ComapR where

class ComapR p where comapr :: (b -> x) -> p a x -> p a b
