module Swap where

-- | swap < swap = id
class Swap f where swap :: f a b -> f b a
