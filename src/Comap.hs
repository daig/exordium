module Comap where
import Bool

class Comap f where comap :: (b -> a) -> f a -> f b
comapId :: (Comap f, Eq (f a)) => f a -> Bool
comapId a = comap (\x -> x) a == a
{-distrib :: (Comap f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool-}
{-distrib f g a = comap (\x -> g (f x)) a == comap g (map f a)-}