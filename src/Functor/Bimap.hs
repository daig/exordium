module Functor.Bimap (Bimap(..), module X) where
import Functor.Map as X
import {-# source #-} E

-- | Independently Map each on both sides
class Bimap p where
  {-# minimal bimap | lmap | rmap #-}
  bimap :: (x -> a) -> (y -> b) -> p x y -> p a b
  bimap f g p = lmap f (rmap g p)
  lmap :: (x -> a) -> p x b -> p a b
  lmap = (`bimap` \x -> x)
  rmap :: (y -> b) -> p a y -> p a b
  rmap = bimap (\x -> x)
  {-bimap f g p = case map f (Flip (map g p)) of Flip fab -> fab -}


instance Bimap (,) where bimap f g (x,y) = (f x, g y)

instance Bimap E where
  bimap f g = \case
    L a -> L (f a)
    R b -> R (g b)
