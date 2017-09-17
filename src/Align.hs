module Align where
import Map
import Plus
import PlusF
import TimesF
import These
import Bool
import Swap

class Map f => Align f where
  {-# minimal (|||) | align #-}
  (|||) :: f a -> f b -> f (These a b)
  (|||) = align This That These
  align :: (a -> c) -> (b -> c) -> (a -> b -> c) -> f a -> f b -> f c
  align f g h a b = map go (a ||| b) where
    go = \case
      This x -> f x
      That y -> g y
      These x y-> h x y

-- | Default definition for (+) @(f a)
plusDefault :: (Align f, Plus a) => f a -> f a -> f a
plusDefault = align (\x -> x) (\x -> x) (+) 


-- Laws
commutes :: (Align f, Eq (f (These a b))) => f a -> f b -> Bool
commutes fa fb = (fa ||| fb) == map swap (fb ||| fa)
alignEmpty :: forall f a b. (Empty f, Align f, Eq (f (These a b))) => f a -> Bool
alignEmpty fa = (fa ||| empty @f @b) == map This fa
