module ComapR where

class ComapR p where comapR :: (b -> x) -> p a x -> p a b
