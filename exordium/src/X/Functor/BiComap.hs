module X.Functor.BiComap (BiComap(..)) where

class BiComap p where
  bicomap :: (a -> x) -> (b -> y) -> p x y -> p a b
  bicomap f g p = cormap g (colmap f p)
  colmap :: (a -> x) -> p x b -> p a b
  colmap = (`bicomap` \x -> x)
  cormap :: (b -> y) -> p a y -> p a b
  cormap = bicomap (\x -> x)
