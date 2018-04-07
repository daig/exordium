module X.Functor.Throw where

-- | hmap eb _ . throw = pure . eb
class Throw e f where throw :: e -> f a
