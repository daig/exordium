module Comap where

class Comap f where comap :: (b -> a) -> f a -> f b
