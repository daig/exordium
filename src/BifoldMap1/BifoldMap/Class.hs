module BifoldMap.Class (BifoldMap(..), module X) where
import PlusZero.Class as X
import List
import Tuple

class BifoldMap t where
  {-# minimal bifoldMap | bifoldr #-}
  bifoldMap :: PlusZero m => (a -> m) -> (b -> m) -> t a b -> m
  bifoldMap f g t = bifoldr (\a m -> f a `plus` m) (\b m -> g b `plus` m) zero t -- TODO: check the order
  bifoldr :: (a -> x -> x) -> (b -> x -> x) -> x -> t a b -> x
  bifoldr c k z t = bifoldMap c k t z

instance BifoldMap (,) where bifoldMap f g (a,b) = f a `plus` g b
