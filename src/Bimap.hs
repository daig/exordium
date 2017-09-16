module Bimap where

class Bimap p where
  {-# minimal bimap | lmap, rmap #-}
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = rmap g (lmap f p)
  lmap :: (x -> a) -> p x b -> p a b
  lmap f = bimap f (\x -> x)
  rmap :: (y -> b) -> p a y -> p a b
  rmap = bimap (\x -> x)
